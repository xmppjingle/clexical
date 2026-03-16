%%
%% http_attend_handler — POST /api/v1/attend
%%
%% Full round-trip: hear the letter (recall stored predicates and pronounce)
%% then proclaim the reply.  Returns 202 Accepted immediately.
%% Accepts both JSON (application/json) and XML (application/xml / text/xml).
%%

-module(http_attend_handler).

-include("../include/clexical.hrl").

-export([init/2]).

init(Req0, State) ->
    case http_auth:require_auth(Req0) of
        {halt, Req} -> {ok, Req, State};
        {ok, Req0}  ->
            handle(Req0, State)
    end.

handle(Req0, State) ->
    case cowboy_req:method(Req0) of
        <<"POST">> ->
            {ok, Body, Req1} = cowboy_req:read_body(Req0),
            case http_herald:parse_body(Body, Req1) of
                undefined ->
                    reply(400, #{<<"error">> => <<"invalid_body">>}, Req1, State);
                #letter{} = Letter ->
                    clexical:attend(Letter),
                    reply(202, #{<<"status">> => <<"attending">>}, Req1, State)
            end;
        _ ->
            reply(405, #{<<"error">> => <<"method_not_allowed">>}, Req0, State)
    end.

reply(Status, Map, Req0, State) ->
    Req = cowboy_req:reply(Status,
        #{<<"content-type">> => <<"application/json">>},
        jsone:encode(Map), Req0),
    {ok, Req, State}.
