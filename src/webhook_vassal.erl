%%
%% webhook_vassal — Vassal that handles outbound webhook action predicates.
%%
%% Implements the vassal behaviour.
%%
%% Verbs recognised when dispatched through clexical:
%%   "do:webhook"      — fire an outbound HTTP request (GET or POST)
%%   "registerWebhook" — register an inbound webhook endpoint
%%   "removeWebhook"   — remove an inbound webhook registration
%%
%% Outbound do:webhook adjectives:
%%   url      (binary)  — target URL (required)
%%   method   (binary)  — "GET" | "POST" (default "POST")
%%   headers  (map)     — extra request headers
%%   body     (binary)  — request body (Mustache template, POST only)
%%   timeout  (integer) — request timeout in ms (default 5000)
%%   retries  (integer) — total attempts (default 1)
%%   store_as (binary)  — if set, stores response body in letter adjectives
%%
%% The response body is stored back into the matching predicate's adjectives
%% under the "response" key (and under store_as if specified), so downstream
%% predicates can read it.
%%

-module(webhook_vassal).
-behaviour(vassal).
-behaviour(gen_server).

-include("../include/clexical.hrl").
-include("../include/clexical_webhooks.hrl").

%% vassal callbacks
-export([initialize/1, work/2]).

%% Public API
-export([register_endpoint/1, remove_endpoint/1, list_endpoints/0, get_endpoint/1]).

%% gen_server
-export([start_link/1, init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

-record(state, {}).

%% ---------------------------------------------------------------------------
%% vassal behaviour
%% ---------------------------------------------------------------------------

initialize(_Opts) -> ok.

work(#letter{predicates = [#predicate{action = {verb, <<"do:webhook">>},
                                       adjectives = Adjs} = P | Rest]} = Letter, _LP) ->
    UpdatedAdjs = fire_webhook(Adjs),
    Letter#letter{predicates = [P#predicate{adjectives = UpdatedAdjs} | Rest]};

work(#letter{predicates = [#predicate{action = {verb, <<"registerWebhook">>},
                                       adjectives = Adjs} | _]} = Letter, _LP) ->
    case register_endpoint(Adjs) of
        {ok, _Id} -> ok;
        {error, R} -> logger:warning("webhook_vassal: register failed: ~p", [R])
    end,
    Letter;

work(#letter{predicates = [#predicate{action = {verb, <<"removeWebhook">>},
                                       adjectives = Adjs} | _]} = Letter, _LP) ->
    Id = maps:get(<<"id">>, Adjs, undefined),
    remove_endpoint(Id),
    Letter;

work(Letter, _LP) ->
    Letter.

%% ---------------------------------------------------------------------------
%% Public API
%% ---------------------------------------------------------------------------

-spec register_endpoint(map()) -> {ok, binary()} | {error, binary()}.
register_endpoint(Map) when is_map(Map) ->
    gen_server:call(?MODULE, {register, Map}).

-spec remove_endpoint(binary() | undefined) -> ok | {error, not_found}.
remove_endpoint(undefined) -> {error, not_found};
remove_endpoint(Id)        -> gen_server:call(?MODULE, {remove, Id}).

-spec list_endpoints() -> [map()].
list_endpoints() ->
    gen_server:call(?MODULE, list).

-spec get_endpoint(binary()) -> {ok, map()} | {error, not_found}.
get_endpoint(Id) ->
    gen_server:call(?MODULE, {get, Id}).

%% ---------------------------------------------------------------------------
%% gen_server
%% ---------------------------------------------------------------------------

start_link(Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Opts, []).

init(_Opts) ->
    ensure_mnesia_table(),
    {ok, #state{}}.

handle_call({register, Map}, _From, State) ->
    case build_reg(Map) of
        {ok, Reg} ->
            ok = mnesia:dirty_write(Reg),
            {reply, {ok, Reg#webhook_reg.id}, State};
        {error, R} ->
            {reply, {error, R}, State}
    end;

handle_call({remove, Id}, _From, State) ->
    case mnesia:dirty_read(webhook_reg, Id) of
        [] -> {reply, {error, not_found}, State};
        [_] ->
            ok = mnesia:dirty_delete(webhook_reg, Id),
            {reply, ok, State}
    end;

handle_call(list, _From, State) ->
    Regs = all_regs(),
    {reply, [reg_to_map(R) || R <- Regs], State};

handle_call({get, Id}, _From, State) ->
    case mnesia:dirty_read(webhook_reg, Id) of
        [R] -> {reply, {ok, reg_to_map(R)}, State};
        []  -> {reply, {error, not_found}, State}
    end;

handle_call(_Req, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% ---------------------------------------------------------------------------
%% Outbound webhook execution
%% ---------------------------------------------------------------------------

fire_webhook(Adjs) ->
    Url = maps:get(<<"url">>, Adjs, undefined),
    case Url of
        undefined ->
            logger:warning("webhook_vassal: do:webhook missing url"),
            maps:put(<<"error">>, <<"missing url">>, Adjs);
        _ ->
            Opts = #{
                url      => Url,
                method   => maps:get(<<"method">>,  Adjs, <<"POST">>),
                headers  => maps:get(<<"headers">>, Adjs, #{}),
                body     => maps:get(<<"body">>,    Adjs, <<>>),
                timeout  => maps:get(<<"timeout">>, Adjs, 5000),
                retries  => maps:get(<<"retries">>, Adjs, 1),
                context  => Adjs
            },
            case webhook_client:call(Opts) of
                {ok, Status, RespHdrs, RespBody} ->
                    StoreAs = maps:get(<<"store_as">>, Adjs, undefined),
                    A1 = maps:put(<<"response">>, RespBody, Adjs),
                    A2 = maps:put(<<"response_status">>, Status, A1),
                    A3 = maps:put(<<"response_headers">>, RespHdrs, A2),
                    case StoreAs of
                        undefined -> A3;
                        Key       -> maps:put(Key, RespBody, A3)
                    end;
                {error, Reason} ->
                    logger:warning("webhook_vassal: outbound call failed: ~p url=~s", [Reason, Url]),
                    maps:put(<<"error">>, term_to_binary(Reason), Adjs)
            end
    end.

%% ---------------------------------------------------------------------------
%% Mnesia helpers
%% ---------------------------------------------------------------------------

ensure_mnesia_table() ->
    mnesia:start(),
    case mnesia:create_table(webhook_reg,
             [{attributes, record_info(fields, webhook_reg)},
              {disc_copies, [node()]}]) of
        {atomic, ok}                   -> ok;
        {aborted, {already_exists, _}} -> ok
    end.

all_regs() ->
    Keys = mnesia:dirty_all_keys(webhook_reg),
    lists:filtermap(fun(Key) ->
        case mnesia:dirty_read(webhook_reg, Key) of
            [#webhook_reg{} = R] -> {true, R};
            _                    -> false
        end
    end, Keys).

build_reg(Map) ->
    Id = maps:get(<<"id">>, Map, fresh_id()),
    case is_binary(Id) andalso byte_size(Id) > 0 of
        false -> {error, <<"id must be a non-empty binary">>};
        true  ->
            {ok, #webhook_reg{
                id               = Id,
                description      = maps:get(<<"description">>, Map, <<>>),
                secret           = maps:get(<<"secret">>,      Map, undefined),
                letter_template  = maps:get(<<"letter">>,      Map, #{}),
                created_at       = erlang:system_time(millisecond),
                enabled          = maps:get(<<"enabled">>,     Map, true)
            }}
    end.

reg_to_map(#webhook_reg{} = R) ->
    Base = #{
        <<"id">>          => R#webhook_reg.id,
        <<"description">> => R#webhook_reg.description,
        <<"letter">>      => R#webhook_reg.letter_template,
        <<"created_at">>  => R#webhook_reg.created_at,
        <<"enabled">>     => R#webhook_reg.enabled
    },
    %% never expose secret in API responses
    case R#webhook_reg.secret of
        undefined -> Base;
        _         -> maps:put(<<"has_secret">>, true, Base)
    end.

fresh_id() ->
    binary:encode_hex(crypto:strong_rand_bytes(8)).
