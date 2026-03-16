%%
%% http_letters_handler — /api/v1/letters
%%
%%   GET  → return recently stored predicates (query by subject/action via QS)
%%   POST → submit a decree letter (stored via prepositions, executed via verbs)
%%

-module(http_letters_handler).

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
    Qs = cowboy_req:parse_qs(Req0),
    Subject = proplists:get_value(<<"subject">>, Qs, ?ANY_SUBJECT),
    Action  = proplists:get_value(<<"action">>,  Qs, ?ANY_SUBJECT),
    Id      = proplists:get_value(<<"id">>,      Qs, ?ANY_ID),
    Key = clexical:compose_key(#predicate{
        subject = Subject,
        id      = Id,
        action  = {verb, Action}
    }),
    Result = case mnesia_scribe:recall(Key) of
        undefined -> [];
        #predicate{} = P -> [http_herald:predicate_to_map(P)]
    end,
    reply_json(200, #{<<"predicates">> => Result}, Req0, State);

handle(<<"POST">>, Req0, State) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    case http_herald:letter_from_binary(Body) of
        undefined ->
            reply_json(400, #{<<"error">> => <<"invalid_json">>}, Req1, State);
        #letter{type = decree} = Letter ->
            clexical:recite(Letter),
            reply_json(202, #{<<"status">> => <<"accepted">>}, Req1, State);
        #letter{type = bulletin} = Letter ->
            clexical:attend(Letter),
            reply_json(202, #{<<"status">> => <<"accepted">>}, Req1, State)
    end;

handle(Method, Req0, State) ->
    reply_json(405, #{<<"error">> => <<"method_not_allowed">>, <<"method">> => Method}, Req0, State).

reply_json(Status, Map, Req0, State) ->
    Req = cowboy_req:reply(Status,
        #{<<"content-type">> => <<"application/json">>},
        jsone:encode(Map), Req0),
    {ok, Req, State}.
