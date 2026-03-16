%%
%% http_automations_handler — /api/v1/automations[/:id]
%%
%%   GET    /api/v1/automations        → list all permanent automations
%%   POST   /api/v1/automations        → register a new automation
%%   DELETE /api/v1/automations/:id    → remove automation by id
%%
%% Automation JSON schema:
%%   {
%%     "id":          "my-automation",          // optional, auto-generated if absent
%%     "name":        "Hourly price sync",       // human label
%%     "schedule":    {"type": "interval", "ms": 3600000},
%%     "letter": { ... }                         // full letter to dispatch
%%   }
%%
%% Schedule types:
%%   {"type": "interval", "ms": N}    — fire every N milliseconds
%%   {"type": "once",     "at": ISO8601}  — fire once at a given time (best-effort)
%%

-module(http_automations_handler).

-include("../include/clexical.hrl").

-export([init/2]).

init(Req0, State) ->
    case http_auth:require_auth(Req0) of
        {halt, Req} -> {ok, Req, State};
        {ok, Req0}  ->
            Method = cowboy_req:method(Req0),
            handle(Method, Req0, State)
    end.

handle(<<"GET">>, Req0, State) ->
    Automations = automation_vassal:list(),
    Body = jsone:encode(#{<<"automations">> => Automations}),
    reply(200, Body, Req0, State);

handle(<<"POST">>, Req0, State) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    try
        Map = jsone:decode(Body, [{object_format, map}]),
        case automation_vassal:register(Map) of
            {ok, Id} ->
                reply(201, jsone:encode(#{<<"id">> => Id, <<"status">> => <<"registered">>}), Req1, State);
            {error, Reason} ->
                reply(422, jsone:encode(#{<<"error">> => Reason}), Req1, State)
        end
    catch
        _:_ ->
            reply(400, jsone:encode(#{<<"error">> => <<"invalid_json">>}), Req1, State)
    end;

handle(<<"DELETE">>, Req0, State) ->
    Id = cowboy_req:binding(id, Req0),
    case automation_vassal:unregister(Id) of
        ok ->
            reply(200, jsone:encode(#{<<"status">> => <<"removed">>}), Req0, State);
        {error, not_found} ->
            reply(404, jsone:encode(#{<<"error">> => <<"not_found">>}), Req0, State)
    end;

handle(_, Req0, State) ->
    reply(405, jsone:encode(#{<<"error">> => <<"method_not_allowed">>}), Req0, State).

reply(Status, Body, Req0, State) ->
    Req = cowboy_req:reply(Status,
        #{<<"content-type">> => <<"application/json">>},
        Body, Req0),
    {ok, Req, State}.
