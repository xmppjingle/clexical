%%
%% automation_vassal — Permanent automation dispatcher.
%%
%% Implements the vassal behaviour. Automations are persisted to Mnesia
%% (disc_copies) so they survive node restarts.
%%
%% Each automation carries:
%%   id        — unique binary identifier
%%   name      — human label
%%   schedule  — {interval, Ms} | {once, EpochMs}
%%   letter    — the #letter{} dispatched on every tick
%%   enabled   — boolean
%%
%% The gen_server is started and supervised by clexical_services_sup.
%% The vassal:initialize/1 callback is therefore a no-op.
%%

-module(automation_vassal).
-behaviour(vassal).
-behaviour(gen_server).

-include("../include/clexical.hrl").

%% vassal callbacks
-export([initialize/1, work/2]).

%% Public API
-compile({no_auto_import, [unregister/1]}).

-export([register/1, unregister/1, list/0, enable/1, disable/1]).

%% gen_server
-export([start_link/1, init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

-record(automation, {
    id       :: binary(),
    name     :: binary(),
    schedule :: {interval, pos_integer()} | {once, pos_integer()},
    letter   :: #letter{},
    enabled  = true :: boolean()
}).

-record(state, {
    timers = #{} :: #{binary() => reference()}
}).

%% ---------------------------------------------------------------------------
%% vassal behaviour — startup is managed by the supervisor via start_link/1
%% ---------------------------------------------------------------------------

initialize(_Opts) -> ok.

%% Verbs recognised when dispatched through clexical:
%%   <<"registerAutomation">>  — register from predicate adjectives
%%   <<"removeAutomation">>    — remove by id
%%   <<"triggerAutomation">>   — manually fire by id
%%   _                         — pass letter through unchanged
work(#letter{predicates = [#predicate{action = {verb, <<"registerAutomation">>},
                                       adjectives = Adjs} | _]} = Letter, _LP) ->
    register(Adjs),
    Letter;
work(#letter{predicates = [#predicate{action = {verb, <<"removeAutomation">>},
                                       adjectives = Adjs} | _]} = Letter, _LP) ->
    unregister(maps:get(<<"id">>, Adjs, undefined)),
    Letter;
work(#letter{predicates = [#predicate{action = {verb, <<"triggerAutomation">>},
                                       adjectives = Adjs} | _]} = Letter, _LP) ->
    gen_server:cast(?MODULE, {fire, maps:get(<<"id">>, Adjs, undefined)}),
    Letter;
work(Letter, _LP) ->
    Letter.

%% ---------------------------------------------------------------------------
%% Public API
%% ---------------------------------------------------------------------------

-spec register(map()) -> {ok, binary()} | {error, binary()}.
register(Map) when is_map(Map) ->
    gen_server:call(?MODULE, {register, Map}).

-spec unregister(binary() | undefined) -> ok | {error, not_found}.
unregister(undefined) -> {error, not_found};
unregister(Id)        -> gen_server:call(?MODULE, {unregister, Id}).

-spec list() -> [map()].
list() ->
    gen_server:call(?MODULE, list).

-spec enable(binary()) -> ok | {error, not_found}.
enable(Id)  -> gen_server:call(?MODULE, {set_enabled, Id, true}).

-spec disable(binary()) -> ok | {error, not_found}.
disable(Id) -> gen_server:call(?MODULE, {set_enabled, Id, false}).

%% ---------------------------------------------------------------------------
%% gen_server
%% ---------------------------------------------------------------------------

start_link(Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Opts, []).

init(_Opts) ->
    ensure_mnesia_table(),
    Timers = restore_automations(),
    {ok, #state{timers = Timers}}.

handle_call({register, Map}, _From, State) ->
    case build_automation(Map) of
        {ok, Auto} ->
            ok = mnesia:dirty_write(Auto),
            Timers = schedule(Auto, State#state.timers),
            {reply, {ok, Auto#automation.id}, State#state{timers = Timers}};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({unregister, Id}, _From, State) ->
    case mnesia:dirty_read(automation, Id) of
        [] ->
            {reply, {error, not_found}, State};
        [_] ->
            ok = mnesia:dirty_delete(automation, Id),
            Timers = cancel_timer(Id, State#state.timers),
            {reply, ok, State#state{timers = Timers}}
    end;

handle_call(list, _From, State) ->
    Autos = all_automations(),
    {reply, [automation_to_map(A) || A <- Autos], State};

handle_call({set_enabled, Id, Bool}, _From, State) ->
    case mnesia:dirty_read(automation, Id) of
        [] ->
            {reply, {error, not_found}, State};
        [Auto] ->
            Updated = Auto#automation{enabled = Bool},
            ok = mnesia:dirty_write(Updated),
            Timers = case Bool of
                true  -> schedule(Updated, State#state.timers);
                false -> cancel_timer(Id, State#state.timers)
            end,
            {reply, ok, State#state{timers = Timers}}
    end;

handle_call(_Req, _From, State) ->
    {reply, ok, State}.

handle_cast({fire, Id}, State) when is_binary(Id) ->
    case mnesia:dirty_read(automation, Id) of
        [#automation{letter = Letter}] -> dispatch(Letter);
        _                              -> logger:warning("automation_vassal: unknown id ~p", [Id])
    end,
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({fire_interval, Id}, State) ->
    case mnesia:dirty_read(automation, Id) of
        [#automation{letter = Letter, schedule = {interval, Ms}, enabled = true}] ->
            dispatch(Letter),
            Ref    = erlang:send_after(Ms, self(), {fire_interval, Id}),
            Timers = maps:put(Id, Ref, State#state.timers),
            {noreply, State#state{timers = Timers}};
        _ ->
            %% Disabled or removed — let the timer lapse.
            {noreply, State}
    end;

handle_info({fire_once, Id}, State) ->
    case mnesia:dirty_read(automation, Id) of
        [#automation{letter = Letter}] -> dispatch(Letter);
        _                              -> ok
    end,
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{timers = Timers}) ->
    maps:foreach(fun(_Id, Ref) -> erlang:cancel_timer(Ref) end, Timers),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ---------------------------------------------------------------------------
%% Internal
%% ---------------------------------------------------------------------------

ensure_mnesia_table() ->
    mnesia:start(),
    case mnesia:create_table(automation,
             [{attributes, record_info(fields, automation)},
              {disc_copies, [node()]}]) of
        {atomic, ok}                   -> ok;
        {aborted, {already_exists, _}} -> ok
    end.

restore_automations() ->
    lists:foldl(fun(Auto, Acc) -> schedule(Auto, Acc) end,
                #{}, all_automations()).

%% Use all_keys + dirty_read to avoid needing a compiled matchspec.
all_automations() ->
    Keys = mnesia:dirty_all_keys(automation),
    lists:filtermap(fun(Key) ->
        case mnesia:dirty_read(automation, Key) of
            [#automation{} = A] -> {true, A};
            _                   -> false
        end
    end, Keys).

schedule(#automation{enabled = false}, Timers) ->
    Timers;
schedule(#automation{id = Id, schedule = {interval, Ms}}, Timers) ->
    T = cancel_timer(Id, Timers),
    Ref = erlang:send_after(Ms, self(), {fire_interval, Id}),
    maps:put(Id, Ref, T);
schedule(#automation{id = Id, schedule = {once, EpochMs}}, Timers) ->
    T = cancel_timer(Id, Timers),
    Delay = max(0, EpochMs - erlang:system_time(millisecond)),
    Ref = erlang:send_after(Delay, self(), {fire_once, Id}),
    maps:put(Id, Ref, T);
schedule(_, Timers) ->
    Timers.

cancel_timer(Id, Timers) ->
    case maps:find(Id, Timers) of
        {ok, Ref} -> erlang:cancel_timer(Ref), maps:remove(Id, Timers);
        error     -> Timers
    end.

dispatch(#letter{} = Letter) ->
    clexical:recite(Letter).

build_automation(Map) ->
    Id       = maps:get(<<"id">>,   Map, fresh_id()),
    Name     = maps:get(<<"name">>, Map, <<"unnamed">>),
    Letter   = http_herald:json_to_letter(maps:get(<<"letter">>, Map, #{})),
    case parse_schedule(maps:get(<<"schedule">>, Map, undefined)) of
        {ok, Schedule} ->
            {ok, #automation{id=Id, name=Name, schedule=Schedule,
                             letter=Letter, enabled=true}};
        {error, Reason} ->
            {error, Reason}
    end.

parse_schedule(#{<<"type">> := <<"interval">>, <<"ms">> := Ms})
  when is_integer(Ms), Ms > 0 ->
    {ok, {interval, Ms}};
parse_schedule(#{<<"type">> := <<"once">>, <<"at">> := AtMs})
  when is_integer(AtMs) ->
    {ok, {once, AtMs}};
parse_schedule(_) ->
    {error, <<"invalid or missing schedule: need {type,interval,ms,N} or {type,once,at,EpochMs}">>}.

automation_to_map(#automation{} = A) ->
    #{
        <<"id">>       => A#automation.id,
        <<"name">>     => A#automation.name,
        <<"schedule">> => schedule_to_map(A#automation.schedule),
        <<"enabled">>  => A#automation.enabled,
        <<"letter">>   => http_herald:letter_to_json(A#automation.letter)
    }.

schedule_to_map({interval, Ms})   -> #{<<"type">> => <<"interval">>, <<"ms">> => Ms};
schedule_to_map({once, EpochMs})  -> #{<<"type">> => <<"once">>, <<"at">> => EpochMs}.

fresh_id() ->
    binary:encode_hex(crypto:strong_rand_bytes(8)).
