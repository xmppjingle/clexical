%%
%% http_recite_handler — POST /api/v1/recite
%%
%% Execute verbs in the letter without the hear/recall phase.
%% The vassal executes work directly; responses come back via proclaim.
%%

-module(http_recite_handler).

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
            case http_herald:letter_from_binary(Body) of
                undefined ->
                    reply_json(400, #{<<"error">> => <<"invalid_json">>}, Req1, State);
                #letter{} = Letter ->
                    clexical:recite(Letter),
                    reply_json(202, #{<<"status">> => <<"reciting">>}, Req1, State)
            end;
        _ ->
            reply_json(405, #{<<"error">> => <<"method_not_allowed">>}, Req0, State)
    end.

reply_json(Status, Map, Req0, State) ->
    Req = cowboy_req:reply(Status,
        #{<<"content-type">> => <<"application/json">>},
        jsone:encode(Map), Req0),
    {ok, Req, State}.
