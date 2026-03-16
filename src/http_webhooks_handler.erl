%%
%% http_webhooks_handler — Webhook registration CRUD.
%%
%%   GET    /api/v1/webhooks          → list registered inbound endpoints
%%   POST   /api/v1/webhooks          → register a new endpoint
%%   GET    /api/v1/webhooks/:id      → get a specific endpoint
%%   DELETE /api/v1/webhooks/:id      → remove endpoint
%%
%% All routes require API-key authentication.
%%
%% Webhook registration JSON:
%%   {
%%     "id":          "my-hook",            // slug used in URL (required)
%%     "description": "Price update hook",  // optional
%%     "secret":      "hmac-secret",        // optional; used for signature verification
%%     "enabled":     true,
%%     "letter": {                          // letter template dispatched on each call
%%       "subject":    "price-update",
%%       "author":     "external",
%%       "type":       "decree",
%%       "predicates": [...]
%%     }
%%   }
%%
%% The letter template supports Mustache placeholders: {{field_name}}.
%% For POST webhooks, JSON body fields are available at the top level.
%% Query params are available under {{params.key}}.
%%

-module(http_webhooks_handler).

-include("../include/clexical.hrl").

-export([init/2]).

init(Req0, State) ->
    case http_auth:require_auth(Req0) of
        {halt, Req} -> {ok, Req, State};
        {ok, _}     ->
            Method = cowboy_req:method(Req0),
            Id     = cowboy_req:binding(id, Req0),
            handle(Method, Id, Req0, State)
    end.

handle(<<"GET">>, undefined, Req0, State) ->
    Endpoints = webhook_vassal:list_endpoints(),
    reply(200, #{<<"webhooks">> => Endpoints}, Req0, State);

handle(<<"GET">>, Id, Req0, State) ->
    case webhook_vassal:get_endpoint(Id) of
        {ok, Map}          -> reply(200, Map, Req0, State);
        {error, not_found} -> reply(404, #{<<"error">> => <<"not_found">>}, Req0, State)
    end;

handle(<<"POST">>, _Id, Req0, State) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    try
        Map = jsone:decode(Body, [{object_format, map}]),
        case webhook_vassal:register_endpoint(Map) of
            {ok, Id} ->
                reply(201, #{<<"id">> => Id, <<"status">> => <<"registered">>}, Req1, State);
            {error, Reason} ->
                reply(422, #{<<"error">> => Reason}, Req1, State)
        end
    catch
        _:_ ->
            reply(400, #{<<"error">> => <<"invalid_json">>}, Req1, State)
    end;

handle(<<"DELETE">>, undefined, Req0, State) ->
    reply(400, #{<<"error">> => <<"id required">>}, Req0, State);

handle(<<"DELETE">>, Id, Req0, State) ->
    case webhook_vassal:remove_endpoint(Id) of
        ok                 -> reply(200, #{<<"status">> => <<"removed">>}, Req0, State);
        {error, not_found} -> reply(404, #{<<"error">> => <<"not_found">>}, Req0, State)
    end;

handle(_, _, Req0, State) ->
    reply(405, #{<<"error">> => <<"method_not_allowed">>}, Req0, State).

reply(Status, Body, Req0, State) ->
    Req = cowboy_req:reply(Status,
        #{<<"content-type">> => <<"application/json">>},
        jsone:encode(Body), Req0),
    {ok, Req, State}.
