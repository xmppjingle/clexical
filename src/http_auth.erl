%%
%% http_auth — API key validation helpers used by all HTTP handlers.
%%
%% The API key is checked in this priority order:
%%   1. X-Api-Key header
%%   2. Authorization: Bearer <key> header
%%   3. api_key query parameter
%%

-module(http_auth).

-export([authenticate/1, require_auth/1]).

%% Returns ok | {error, Req} (with 401 already written to Req).
-spec authenticate(cowboy_req:req()) -> ok | {error, cowboy_req:req()}.
authenticate(Req) ->
    Expected = persistent_term:get(clexical_api_key, <<"changeme">>),
    case extract_key(Req) of
        Expected ->
            ok;
        _ ->
            {error, reply_unauthorized(Req)}
    end.

%% Sugar for handlers: returns {ok, Req} or halts with 401.
-spec require_auth(cowboy_req:req()) -> {ok, cowboy_req:req()} | {halt, cowboy_req:req()}.
require_auth(Req) ->
    case authenticate(Req) of
        ok          -> {ok, Req};
        {error, R}  -> {halt, R}
    end.

%% ---------------------------------------------------------------------------
%% Internal
%% ---------------------------------------------------------------------------

extract_key(Req) ->
    case cowboy_req:header(<<"x-api-key">>, Req) of
        undefined ->
            case cowboy_req:header(<<"authorization">>, Req) of
                <<"Bearer ", Key/binary>> -> Key;
                _ ->
                    #{api_key := QsKey} = cowboy_req:match_qs([{api_key, [], undefined}], Req),
                    QsKey
            end;
        Key -> Key
    end.

reply_unauthorized(Req) ->
    cowboy_req:reply(401, #{<<"content-type">> => <<"application/json">>},
        jsone:encode(#{<<"error">> => <<"unauthorized">>, <<"message">> => <<"Invalid or missing API key">>}),
        Req).
