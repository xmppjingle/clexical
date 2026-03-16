%%
%% automation_vassal — Permanent automation dispatcher.
%%
%% Implements the vassal behaviour and doubles as an automation registry.
%% Automations survive node restarts because they are persisted in Mnesia.
%%
%% Each automation record holds:
%%   id        — unique binary identifier
%%   name      — human-readable label
%%   schedule  — {interval, Ms} | {once, EpochMs}
%%   letter    — the #letter{} to dispatch on each tick
%%   enabled   — boolean
%%
%% On startup, all enabled automations are restored and their timers restarted.
%%

-module(automation_vassal).
-behaviour(vassal).
-behaviour(gen_server).

-include("../include/clexical.hrl").

%% vassal callbacks
-export([initialize/1, work/2]).

%% Public API
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
%% vassal behaviour
%% ---------------------------------------------------------------------------

initialize(Opts) ->
    {ok, _} = start_link(Opts),
    ok.

%% Called by clexical engine when a verb is dispatched to this vassal.
%% Verbs recognised:
%%   <<"registerAutomation">>  — register an automation from predicate adjectives
%%   <<"removeAutomation">>    — remove by id (in adjectives)
%%   <<"triggerAutomation">>   — manually fire an automation by id
%%   _                         — no-op, return empty letter
work(#letter{predicates = [#predicate{action = {verb, <<"registerAutomation">>},
                                       adjectives = Adjs} | _]} = Letter,
     _LP) ->
    case register(Adjs) of
        {ok, _Id} -> Letter;
        _         -> Letter
    end;
work(#letter{predicates = [#predicate{action = {verb, <<"removeAutomation">>},
                                       adjectives = Adjs} | _]} = Letter,
     _LP) ->
    Id = maps:get(<<"id">>, Adjs, undefined),
    unregister(Id),
    Letter;
work(#letter{predicates = [#predicate{action = {verb, <<"triggerAutomation">>},
                                       adjectives = Adjs} | _]} = Letter,
     _LP) ->
    Id = maps:get(<<"id">>, Adjs, undefined),
    gen_server:cast(?MODULE, {fire, Id}),
    Letter;
work(Letter, _LP) ->
    Letter.

%% ---------------------------------------------------------------------------
%% Public API
%% ---------------------------------------------------------------------------

-spec register(map()) -> {ok, binary()} | {error, binary()}.
register(Map) when is_map(Map) ->
    gen_server:call(?MODULE, {register, Map}).

-spec unregister(binary()) -> ok | {error, not_found}.
unregister(Id) ->
    gen_server:call(?MODULE, {unregister, Id}).

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
    %% Ensure the Mnesia automation table exists.
    case mnesia:create_table(automation,
             [{attributes, record_info(fields, automation)},
              {disc_copies, [node()]}]) of
        {atomic, ok}          -> ok;
        {aborted, {already_exists, _}} -> ok
    end,
    %% Restore all enabled automations.
    Timers = restore_automations(),
    {ok, #state{timers = Timers}}.

handle_call({register, Map}, _From, State) ->
    case build_automation(Map) of
        {ok, Auto} ->
            ok = mnesia:dirty_write(Auto),
            Timers = maybe_schedule(Auto, State#state.timers),
            {reply, {ok, Auto#automation.id}, State#state{timers = Timers}};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({unregister, Id}, _From, State) ->
    case mnesia:dirty_read(automation, Id) of
        [] ->
            {reply, {error, not_found}, State};
        [_Auto] ->
            ok = mnesia:dirty_delete(automation, Id),
            Timers = cancel_timer(Id, State#state.timers),
            {reply, ok, State#state{timers = Timers}}
    end;

handle_call(list, _From, State) ->
    Records = mnesia:dirty_match_object(#automation{_ = '_'}),
    Maps = [automation_to_map(A) || A <- Records],
    {reply, Maps, State};

handle_call({set_enabled, Id, Bool}, _From, State) ->
    case mnesia:dirty_read(automation, Id) of
        [] ->
            {reply, {error, not_found}, State};
        [Auto] ->
            Updated = Auto#automation{enabled = Bool},
            ok = mnesia:dirty_write(Updated),
            Timers = case Bool of
                true  -> maybe_schedule(Updated, State#state.timers);
                false -> cancel_timer(Id, State#state.timers)
            end,
            {reply, ok, State#state{timers = Timers}}
    end;

handle_call(_Req, _From, State) ->
    {reply, ok, State}.

handle_cast({fire, Id}, State) ->
    case mnesia:dirty_read(automation, Id) of
        [#automation{letter = Letter}] ->
            dispatch(Letter);
        _ ->
            logger:warning("automation_vassal: fire unknown id ~p", [Id])
    end,
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({fire_interval, Id}, State) ->
    case mnesia:dirty_read(automation, Id) of
        [#automation{letter = Letter, schedule = {interval, Ms}, enabled = true}] ->
            dispatch(Letter),
            Ref = erlang:send_after(Ms, self(), {fire_interval, Id}),
            Timers = maps:put(Id, Ref, State#state.timers),
            {noreply, State#state{timers = Timers}};
        _ ->
            %% Disabled or removed — let the timer die.
            {noreply, State}
    end;

handle_info({fire_once, Id}, State) ->
    case mnesia:dirty_read(automation, Id) of
        [#automation{letter = Letter}] ->
            dispatch(Letter);
        _ ->
            ok
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
%% Internal helpers
%% ---------------------------------------------------------------------------

restore_automations() ->
    Records = mnesia:dirty_match_object(#automation{_ = '_'}),
    lists:foldl(fun(Auto, Acc) ->
        maybe_schedule(Auto, Acc)
    end, #{}, Records).

maybe_schedule(#automation{id = Id, enabled = false}, Timers) ->
    Timers;
maybe_schedule(#automation{id = Id, schedule = {interval, Ms}}, Timers) ->
    cancel_timer(Id, Timers),
    Ref = erlang:send_after(Ms, self(), {fire_interval, Id}),
    maps:put(Id, Ref, Timers);
maybe_schedule(#automation{id = Id, schedule = {once, EpochMs}}, Timers) ->
    Now = erlang:system_time(millisecond),
    Delay = max(0, EpochMs - Now),
    Ref = erlang:send_after(Delay, self(), {fire_once, Id}),
    maps:put(Id, Ref, Timers);
maybe_schedule(_, Timers) ->
    Timers.

cancel_timer(Id, Timers) ->
    case maps:find(Id, Timers) of
        {ok, Ref} ->
            erlang:cancel_timer(Ref),
            maps:remove(Id, Timers);
        error ->
            Timers
    end.

dispatch(#letter{} = Letter) ->
    clexical:recite(Letter).

build_automation(Map) ->
    Id = maps:get(<<"id">>, Map, fresh_id()),
    Name = maps:get(<<"name">>, Map, <<"unnamed">>),
    LetterMap = maps:get(<<"letter">>, Map, #{}),
    Letter = http_herald:json_to_letter(LetterMap),
    case parse_schedule(maps:get(<<"schedule">>, Map, undefined)) of
        {ok, Schedule} ->
            {ok, #automation{
                id       = Id,
                name     = Name,
                schedule = Schedule,
                letter   = Letter,
                enabled  = true
            }};
        {error, Reason} ->
            {error, Reason}
    end.

parse_schedule(#{<<"type">> := <<"interval">>, <<"ms">> := Ms}) when is_integer(Ms), Ms > 0 ->
    {ok, {interval, Ms}};
parse_schedule(#{<<"type">> := <<"once">>, <<"at">> := AtMs}) when is_integer(AtMs) ->
    {ok, {once, AtMs}};
parse_schedule(_) ->
    {error, <<"invalid or missing schedule">>}.

automation_to_map(#automation{} = A) ->
    #{
        <<"id">>      => A#automation.id,
        <<"name">>    => A#automation.name,
        <<"schedule">> => schedule_to_map(A#automation.schedule),
        <<"enabled">> => A#automation.enabled,
        <<"letter">>  => http_herald:letter_to_json(A#automation.letter)
    }.

schedule_to_map({interval, Ms}) ->
    #{<<"type">> => <<"interval">>, <<"ms">> => Ms};
schedule_to_map({once, EpochMs}) ->
    #{<<"type">> => <<"once">>, <<"at">> => EpochMs}.

fresh_id() ->
    Rand = crypto:strong_rand_bytes(8),
    binary:encode_hex(Rand).
