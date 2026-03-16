%%
%% ct_helper — Shared utilities for all clexical Common Test suites.
%%
-module(ct_helper).

-export([
    start_app/2,
    stop_app/0,
    base_url/2,
    api_url/2,
    http_get/2,
    http_post/3,
    http_delete/2,
    http_bearer/3,
    http_qs_key/3,
    http_no_auth_get/1,
    http_no_auth_post/2,
    http_hmac_post/4,
    status/1,
    body/1,
    decode_body/1,
    make_letter/1,
    make_letter/2,
    make_letter/3,
    make_decree/3,
    make_bulletin/3,
    make_predicate/2,
    make_predicate/3,
    make_verb/1,
    make_verb/2,
    make_prep/2,
    unique_id/1,
    wait_for/3
]).

-define(HTTP_TIMEOUT, 8000).

%% ---------------------------------------------------------------------------
%% Application lifecycle
%% ---------------------------------------------------------------------------

start_app(Port, ApiKey) ->
    TestDir = "/tmp/clexical_ct_" ++ integer_to_list(erlang:unique_integer([positive, monotonic])),
    os:putenv("CLEXICAL_MNESIA_DIR", TestDir),
    %% Stop mnesia if running from a previous suite
    mnesia:stop(),
    application:set_env(mnesia, dir, TestDir),
    case mnesia:create_schema([node()]) of
        ok                                    -> ok;
        {error, {_, {already_exists, _}}}     -> ok
    end,
    application:set_env(clexical, config, [
        {herald, {http_herald,       #{port => Port, api_key => ApiKey}}},
        {scribe, {mnesia_scribe,     #{}}},
        {vassal, {automation_vassal, #{}}}
    ]),
    {ok, _} = application:ensure_all_started(clexical),
    {ok, _} = application:ensure_all_started(inets),
    {ok, _} = application:ensure_all_started(ssl),
    ok.

stop_app() ->
    application:stop(clexical),
    mnesia:stop(),
    ok.

%% ---------------------------------------------------------------------------
%% URL helpers
%% ---------------------------------------------------------------------------

base_url(Port, Path) ->
    "http://localhost:" ++ integer_to_list(Port) ++ Path.

api_url(Port, Path) ->
    base_url(Port, "/api/v1" ++ Path).

%% ---------------------------------------------------------------------------
%% HTTP client helpers (all return raw httpc response)
%% ---------------------------------------------------------------------------

http_get(Url, Key) when is_binary(Key) ->
    httpc:request(get,
        {Url, [{"x-api-key", binary_to_list(Key)}]},
        [{timeout, ?HTTP_TIMEOUT}], [{body_format, binary}]);
http_get(Url, Key) ->
    httpc:request(get,
        {Url, [{"x-api-key", Key}]},
        [{timeout, ?HTTP_TIMEOUT}], [{body_format, binary}]).

http_post(Url, Key, Body) when is_binary(Key) ->
    httpc:request(post,
        {Url, [{"x-api-key", binary_to_list(Key)}], "application/json", Body},
        [{timeout, ?HTTP_TIMEOUT}], [{body_format, binary}]);
http_post(Url, Key, Body) ->
    httpc:request(post,
        {Url, [{"x-api-key", Key}], "application/json", Body},
        [{timeout, ?HTTP_TIMEOUT}], [{body_format, binary}]).

http_delete(Url, Key) when is_binary(Key) ->
    httpc:request(delete,
        {Url, [{"x-api-key", binary_to_list(Key)}]},
        [{timeout, ?HTTP_TIMEOUT}], [{body_format, binary}]);
http_delete(Url, Key) ->
    httpc:request(delete,
        {Url, [{"x-api-key", Key}]},
        [{timeout, ?HTTP_TIMEOUT}], [{body_format, binary}]).

http_bearer(Url, Key, Body) when is_binary(Key) ->
    http_bearer(Url, binary_to_list(Key), Body);
http_bearer(Url, Key, Body) ->
    httpc:request(post,
        {Url, [{"authorization", "Bearer " ++ Key}], "application/json", Body},
        [{timeout, ?HTTP_TIMEOUT}], [{body_format, binary}]).

http_qs_key(Url, Key, Body) when is_binary(Key) ->
    http_qs_key(Url, binary_to_list(Key), Body);
http_qs_key(Url, Key, Body) ->
    FullUrl = Url ++ "?api_key=" ++ http_uri_encode(Key),
    httpc:request(post,
        {FullUrl, [], "application/json", Body},
        [{timeout, ?HTTP_TIMEOUT}], [{body_format, binary}]).

http_no_auth_get(Url) ->
    httpc:request(get,
        {Url, []},
        [{timeout, ?HTTP_TIMEOUT}], [{body_format, binary}]).

http_no_auth_post(Url, Body) ->
    httpc:request(post,
        {Url, [], "application/json", Body},
        [{timeout, ?HTTP_TIMEOUT}], [{body_format, binary}]).

http_hmac_post(Url, Secret, Body, ExtraHeaders) ->
    Mac    = crypto:mac(hmac, sha256, Secret, Body),
    SigHex = string:lowercase(binary_to_list(binary:encode_hex(Mac))),
    Sig    = "sha256=" ++ SigHex,
    httpc:request(post,
        {Url, [{"x-webhook-signature", Sig} | ExtraHeaders],
         "application/json", Body},
        [{timeout, ?HTTP_TIMEOUT}], [{body_format, binary}]).

%% ---------------------------------------------------------------------------
%% Response helpers
%% ---------------------------------------------------------------------------

status({ok, {{_, S, _}, _, _}}) -> S.

body({ok, {{_, _, _}, _, B}})   -> B.

decode_body(Resp) ->
    jsone:decode(body(Resp), [{object_format, map}]).

%% ---------------------------------------------------------------------------
%% Letter / Predicate JSON builders
%% ---------------------------------------------------------------------------

make_letter(Subject) ->
    make_decree(Subject, <<"system">>, []).

make_letter(Subject, Predicates) ->
    make_decree(Subject, <<"system">>, Predicates).

make_letter(Subject, Author, Predicates) ->
    make_decree(Subject, Author, Predicates).

make_decree(Subject, Author, Predicates) ->
    jsone:encode(#{
        <<"subject">>    => Subject,
        <<"author">>     => Author,
        <<"type">>       => <<"decree">>,
        <<"predicates">> => Predicates
    }).

make_bulletin(Subject, Author, Predicates) ->
    jsone:encode(#{
        <<"subject">>    => Subject,
        <<"author">>     => Author,
        <<"type">>       => <<"bulletin">>,
        <<"predicates">> => Predicates
    }).

%% Predicate map (for embedding in letters)
make_predicate(ActionType, Action) ->
    make_predicate(ActionType, Action, #{}).

make_predicate(ActionType, Action, Adjectives) ->
    #{
        <<"action_type">> => ActionType,
        <<"action">>      => Action,
        <<"adjectives">>  => Adjectives
    }.

make_verb(Action, Adjectives) ->
    make_predicate(<<"verb">>, Action, Adjectives).

make_verb(Action) ->
    make_predicate(<<"verb">>, Action, #{}).

make_prep(Action, Adjectives) ->
    make_predicate(<<"preposition">>, Action, Adjectives).

%% ---------------------------------------------------------------------------
%% Misc
%% ---------------------------------------------------------------------------

unique_id(Prefix) ->
    N = erlang:unique_integer([positive, monotonic]),
    B = integer_to_binary(N),
    <<Prefix/binary, $-, B/binary>>.

%% Poll Fun until it returns true, or until Timeout ms elapsed.
wait_for(Fun, Timeout, _Interval) when Timeout =< 0 ->
    Fun();
wait_for(Fun, Timeout, Interval) ->
    case (catch Fun()) of
        true -> true;
        _    ->
            timer:sleep(Interval),
            wait_for(Fun, Timeout - Interval, Interval)
    end.

http_uri_encode(S) ->
    binary_to_list(uri_string:quote(list_to_binary(S))).
