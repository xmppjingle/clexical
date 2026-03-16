%%
%% http_herald — Cowboy-based Herald implementation.
%%
%% Implements the herald behaviour over HTTP/REST:
%%
%%   GET  /api/v1/letters          → query stored predicates (bulletin)
%%   POST /api/v1/letters          → submit a decree (stored + executed)
%%   POST /api/v1/attend           → full round-trip (hear + proclaim)
%%   POST /api/v1/recite           → execute verbs only
%%   GET  /api/v1/automations      → list permanent automations
%%   POST /api/v1/automations      → register permanent automation
%%   DELETE /api/v1/automations/:id → remove automation
%%   GET  /api/v1/webhooks         → list inbound webhook endpoints
%%   POST /api/v1/webhooks         → register inbound webhook endpoint
%%   GET  /api/v1/webhooks/:id     → get webhook endpoint by id
%%   DELETE /api/v1/webhooks/:id   → remove webhook endpoint
%%   GET  /api/v1/webhooks/in/:name → trigger inbound webhook (GET)
%%   POST /api/v1/webhooks/in/:name → trigger inbound webhook (POST)
%%   GET  /health                   → health check (no auth)
%%
%% API key authentication is required on all /api/* routes.
%% Supply the key via any of:
%%   Header:  X-Api-Key: <key>
%%   Header:  Authorization: Bearer <key>
%%   QParam:  ?api_key=<key>
%%
%% The gen_server is started and supervised by clexical_services_sup.
%% The herald:initialize/1 callback is therefore a no-op.
%%

-module(http_herald).
-behaviour(herald).
-behaviour(gen_server).

-include("../include/clexical.hrl").

%% herald callbacks
-export([initialize/1, proclaim/1, letter_from_binary/1, to_binary/1, excerpts/1]).

%% gen_server
-export([start_link/1, init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

%% Internal cowboy routing helper
-export([routes/0]).

%% Conversion helpers used by handlers and automation_vassal
-export([json_to_letter/1, letter_to_json/1, predicate_to_map/1, map_to_predicate/1]).

-define(DEFAULT_PORT, 8080).

-record(state, {port, ref}).

%% ---------------------------------------------------------------------------
%% herald behaviour
%% ---------------------------------------------------------------------------

%% No-op: startup is managed by the supervisor via start_link/1.
initialize(_Opts) -> ok.

proclaim(#letter{} = Letter) ->
    logger:debug("http_herald proclaim: ~p", [to_binary(Letter)]),
    clexical_notifier:notify(Letter),
    ok.

letter_from_binary(Bin) when is_binary(Bin) ->
    try
        json_to_letter(jsone:decode(Bin, [{object_format, map}]))
    catch
        _:_ -> undefined
    end.

to_binary(#letter{} = Letter) ->
    jsone:encode(letter_to_json(Letter));
to_binary(_) ->
    <<>>.

%% Excerpts extracts sub-predicates from a predicate's abstract payload.
%% If abstract is a JSON binary carrying a nested letter, its predicates
%% are returned; otherwise the predicate itself is returned as a singleton.
excerpts(#predicate{abstract = Abstract} = P) when is_binary(Abstract) ->
    try
        SubLetter = json_to_letter(jsone:decode(Abstract, [{object_format, map}])),
        SubLetter#letter.predicates
    catch
        _:_ -> [P]
    end;
excerpts(#predicate{abstract = undefined}) ->
    [];
excerpts(#predicate{} = P) ->
    [P].

%% ---------------------------------------------------------------------------
%% gen_server
%% ---------------------------------------------------------------------------

start_link(Opts) when is_map(Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Opts, []);
start_link(Opts) when is_list(Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, maps:from_list(Opts), []).

init(Opts) ->
    Port   = maps:get(port,    Opts, env_int("CLEXICAL_HTTP_PORT", ?DEFAULT_PORT)),
    ApiKey = maps:get(api_key, Opts, os:getenv("CLEXICAL_API_KEY", "changeme")),
    persistent_term:put(clexical_api_key, list_to_binary(ApiKey)),
    Dispatch = cowboy_router:compile([{'_', routes()}]),
    {ok, Ref} = cowboy:start_clear(http_herald_listener,
                    [{port, Port}],
                    #{env => #{dispatch => Dispatch}}),
    logger:info("http_herald listening on port ~p", [Port]),
    {ok, #state{port = Port, ref = Ref}}.

handle_call(_Req, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{ref = Ref}) ->
    cowboy:stop_listener(Ref),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ---------------------------------------------------------------------------
%% Cowboy routes
%% ---------------------------------------------------------------------------

routes() ->
    [
        {"/health",                       http_health_handler,      #{}},
        {"/api/v1/letters",               http_letters_handler,     #{}},
        {"/api/v1/attend",                http_attend_handler,      #{}},
        {"/api/v1/recite",                http_recite_handler,      #{}},
        {"/api/v1/automations",           http_automations_handler, #{}},
        {"/api/v1/automations/:id",       http_automations_handler, #{}},
        %% Inbound webhook trigger (no auth — uses HMAC signature)
        {"/api/v1/webhooks/in/:name",     http_webhook_in_handler,  #{}},
        %% Webhook registration CRUD (requires API key)
        {"/api/v1/webhooks",              http_webhooks_handler,    #{}},
        {"/api/v1/webhooks/:id",          http_webhooks_handler,    #{}}
    ].

%% ---------------------------------------------------------------------------
%% JSON ↔ Record conversion  (used by handlers + automation_vassal)
%% ---------------------------------------------------------------------------

-spec json_to_letter(map()) -> #letter{}.
json_to_letter(Map) when is_map(Map) ->
    Predicates = [map_to_predicate(P)
                  || P <- maps:get(<<"predicates">>, Map, []),
                     is_map(P)],
    #letter{
        subject    = bin(maps:get(<<"subject">>,   Map, <<>>)),
        author     = bin(maps:get(<<"author">>,    Map, <<>>)),
        recipient  = bin(maps:get(<<"recipient">>, Map, <<>>)),
        predicates = Predicates,
        via        = http,
        type       = to_letter_type(maps:get(<<"type">>, Map, <<"decree">>))
    }.

-spec letter_to_json(#letter{} | undefined) -> map().
letter_to_json(#letter{} = L) ->
    #{
        <<"subject">>    => safe_bin(L#letter.subject),
        <<"author">>     => safe_bin(L#letter.author),
        <<"recipient">>  => safe_bin(L#letter.recipient),
        <<"type">>       => atom_to_binary(L#letter.type, utf8),
        <<"predicates">> => [predicate_to_map(P) || P <- L#letter.predicates]
    };
letter_to_json(_) ->
    #{}.

-spec map_to_predicate(map()) -> #predicate{}.
map_to_predicate(Map) when is_map(Map) ->
    Kind = to_action_type(maps:get(<<"action_type">>, Map, <<"verb">>)),
    Name = bin(maps:get(<<"action">>,     Map, <<>>)),
    #predicate{
        id         = bin(maps:get(<<"id">>,         Map, <<>>)),
        subject    = bin(maps:get(<<"subject">>,    Map, <<>>)),
        author     = maps:get(<<"author">>,         Map, undefined),
        action     = {Kind, Name},
        adjectives = maps:get(<<"adjectives">>,     Map, #{}),
        abstract   = maps:get(<<"abstract">>,       Map, undefined)
    }.

-spec predicate_to_map(#predicate{}) -> map().
predicate_to_map(#predicate{action = {Kind, Name}} = P) ->
    #{
        <<"id">>          => safe_bin(P#predicate.id),
        <<"subject">>     => safe_bin(P#predicate.subject),
        <<"action_type">> => atom_to_binary(Kind, utf8),
        <<"action">>      => Name,
        <<"adjectives">>  => P#predicate.adjectives
    }.

%% ---------------------------------------------------------------------------
%% Helpers
%% ---------------------------------------------------------------------------

to_letter_type(<<"bulletin">>) -> bulletin;
to_letter_type(_)              -> decree.

to_action_type(<<"preposition">>) -> preposition;
to_action_type(_)                 -> verb.

bin(B) when is_binary(B) -> B;
bin(L) when is_list(L)   -> list_to_binary(L);
bin(A) when is_atom(A)   -> atom_to_binary(A, utf8);
bin(_)                   -> <<>>.

safe_bin(undefined) -> <<>>;
safe_bin(V)         -> bin(V).

env_int(Var, Default) ->
    case os:getenv(Var) of
        false -> Default;
        S     -> list_to_integer(S)
    end.
