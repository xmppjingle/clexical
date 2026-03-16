%%
%% http_herald — Cowboy-based Herald implementation.
%%
%% Implements the herald behaviour over HTTP/REST:
%%   GET  /api/v1/letters          → query stored predicates (bulletin)
%%   POST /api/v1/letters          → submit a decree (stored + executed)
%%   POST /api/v1/attend           → recite + proclaim (full round-trip)
%%   POST /api/v1/recite           → recite only (execute verbs)
%%   GET  /api/v1/automations      → list permanent automations
%%   POST /api/v1/automations      → register permanent automation
%%   DELETE /api/v1/automations/:id → remove automation
%%   GET  /health                   → health check (no auth required)
%%
%% API key authentication is required on all /api/* routes.
%% Pass the key via:
%%   - Header:  X-Api-Key: <key>
%%   - Header:  Authorization: Bearer <key>
%%   - QParam:  ?api_key=<key>
%%

-module(http_herald).
-behaviour(herald).
-behaviour(gen_server).

-include("../include/clexical.hrl").

%% herald callbacks
-export([initialize/1, proclaim/1, letter_from_binary/1, to_binary/1, excerpts/1]).

%% gen_server callbacks
-export([start_link/1, init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

%% Internal cowboy routing
-export([routes/0]).

%% Public helper used by handlers
-export([json_to_letter/1, letter_to_json/1, predicate_to_map/1, map_to_predicate/1]).

-define(DEFAULT_PORT, 8080).
-define(ENV_PORT, "CLEXICAL_HTTP_PORT").
-define(ENV_KEY,  "CLEXICAL_API_KEY").

-record(state, {port, cowboy_ref}).

%% ---------------------------------------------------------------------------
%% herald behaviour
%% ---------------------------------------------------------------------------

initialize(Opts) ->
    Port = maps:get(port, Opts, env_int(?ENV_PORT, ?DEFAULT_PORT)),
    ApiKey = maps:get(api_key, Opts, os:getenv(?ENV_KEY, "changeme")),
    persistent_term:put(clexical_api_key, list_to_binary(ApiKey)),
    {ok, _} = start_link([{port, Port}]),
    ok.

proclaim(#letter{} = Letter) ->
    %% Broadcast to any registered SSE / webhook subscribers.
    %% For now we log and route back via the automation notifier.
    logger:debug("http_herald proclaim: ~p", [letter_to_json(Letter)]),
    clexical_notifier:notify(Letter),
    ok.

letter_from_binary(Bin) when is_binary(Bin) ->
    try
        Map = jsone:decode(Bin, [{object_format, map}]),
        json_to_letter(Map)
    catch
        _:_ -> undefined
    end.

to_binary(#letter{} = Letter) ->
    jsone:encode(letter_to_json(Letter)).

excerpts(#predicate{abstract = Abstract} = P) when Abstract /= undefined ->
    %% Abstract may be a JSON-encoded sub-letter; extract its predicates.
    try
        SubMap = jsone:decode(Abstract, [{object_format, map}]),
        SubLetter = json_to_letter(SubMap),
        SubLetter#letter.predicates
    catch
        _:_ -> [P]
    end;
excerpts(#predicate{} = P) ->
    [P].

%% ---------------------------------------------------------------------------
%% gen_server
%% ---------------------------------------------------------------------------

start_link(Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Opts, []).

init(Opts) ->
    Port = proplists:get_value(port, Opts, ?DEFAULT_PORT),
    Dispatch = cowboy_router:compile([
        {'_', routes()}
    ]),
    {ok, Ref} = cowboy:start_clear(http_herald_listener,
        [{port, Port}],
        #{env => #{dispatch => Dispatch}}
    ),
    logger:info("http_herald listening on port ~p", [Port]),
    {ok, #state{port = Port, cowboy_ref = Ref}}.

handle_call(_Req, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{cowboy_ref = Ref}) ->
    cowboy:stop_listener(Ref),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ---------------------------------------------------------------------------
%% Cowboy routes
%% ---------------------------------------------------------------------------

routes() ->
    [
        {"/health",                    http_health_handler,     #{}},
        {"/api/v1/letters",            http_letters_handler,    #{}},
        {"/api/v1/attend",             http_attend_handler,     #{}},
        {"/api/v1/recite",             http_recite_handler,     #{}},
        {"/api/v1/automations",        http_automations_handler,#{}},
        {"/api/v1/automations/:id",    http_automations_handler,#{}}
    ].

%% ---------------------------------------------------------------------------
%% JSON ↔ Letter conversion
%% ---------------------------------------------------------------------------

json_to_letter(Map) when is_map(Map) ->
    Predicates = lists:map(fun map_to_predicate/1,
                           maps:get(<<"predicates">>, Map, [])),
    #letter{
        subject   = maps:get(<<"subject">>,   Map, <<>>),
        author    = maps:get(<<"author">>,    Map, <<>>),
        recipient = maps:get(<<"recipient">>, Map, <<>>),
        predicates= Predicates,
        via       = maps:get(<<"via">>,       Map, http),
        type      = binary_to_letter_type(maps:get(<<"type">>, Map, <<"decree">>))
    }.

letter_to_json(#letter{} = L) ->
    #{
        <<"subject">>    => null_to_empty(L#letter.subject),
        <<"author">>     => null_to_empty(L#letter.author),
        <<"recipient">>  => null_to_empty(L#letter.recipient),
        <<"type">>       => atom_to_binary(L#letter.type, utf8),
        <<"predicates">> => lists:map(fun predicate_to_map/1, L#letter.predicates)
    }.

map_to_predicate(Map) when is_map(Map) ->
    ActionType = binary_to_action_type(maps:get(<<"action_type">>, Map, <<"verb">>)),
    ActionName = maps:get(<<"action">>, Map, <<>>),
    #predicate{
        id         = maps:get(<<"id">>,       Map, <<>>),
        subject    = maps:get(<<"subject">>,  Map, <<>>),
        author     = maps:get(<<"author">>,   Map, undefined),
        action     = {ActionType, ActionName},
        adjectives = maps:get(<<"adjectives">>, Map, #{}),
        abstract   = maps:get(<<"abstract">>,   Map, undefined)
    }.

predicate_to_map(#predicate{action = {Kind, Name}} = P) ->
    #{
        <<"id">>          => null_to_empty(P#predicate.id),
        <<"subject">>     => null_to_empty(P#predicate.subject),
        <<"action_type">> => atom_to_binary(Kind, utf8),
        <<"action">>      => Name,
        <<"adjectives">>  => P#predicate.adjectives
    }.

%% ---------------------------------------------------------------------------
%% Helpers
%% ---------------------------------------------------------------------------

binary_to_letter_type(<<"bulletin">>) -> bulletin;
binary_to_letter_type(_)              -> decree.

binary_to_action_type(<<"preposition">>) -> preposition;
binary_to_action_type(_)                 -> verb.

null_to_empty(undefined) -> <<>>;
null_to_empty(V)         -> V.

env_int(Var, Default) ->
    case os:getenv(Var) of
        false -> Default;
        S     -> list_to_integer(S)
    end.
