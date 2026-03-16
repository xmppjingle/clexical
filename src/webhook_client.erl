%%
%% webhook_client — Outbound HTTP client for do:webhook action predicates.
%%
%% Supports GET and POST with:
%%   - Configurable timeout (ms)
%%   - Retry with exponential backoff (2s, 4s, 8s …)
%%   - Custom request headers
%%   - Mustache body template rendering from context map
%%   - Returns {ok, StatusCode, Headers, Body} | {error, Reason}
%%
%% Uses OTP inets/httpc with a NAMED profile (webhook_httpc) so that
%% proxy configuration never leaks into the default httpc profile used
%% by the application or tests.
%%

-module(webhook_client).

-export([call/1, call/2]).

-define(DEFAULT_TIMEOUT, 5000).
-define(DEFAULT_RETRIES, 1).
-define(PROFILE,         webhook_httpc).

%%
%% call(Opts) — Opts is a map with keys:
%%   url      :: binary()    (required)
%%   method   :: binary()    "GET" | "POST"
%%   headers  :: map()       extra headers
%%   body     :: binary()    body template
%%   context  :: map()       render context for Mustache templates
%%   timeout  :: integer()   ms
%%   retries  :: integer()
%%
-spec call(map()) -> {ok, integer(), map(), binary()} | {error, term()}.
call(Opts) ->
    call(Opts, maps:get(retries, Opts, ?DEFAULT_RETRIES)).

-spec call(map(), integer()) -> {ok, integer(), map(), binary()} | {error, term()}.
call(Opts, AttemptsLeft) ->
    Url     = binary_to_list(maps:get(url, Opts)),
    Method  = normalize_method(maps:get(method, Opts, <<"POST">>)),
    Headers = build_headers(maps:get(headers, Opts, #{})),
    Timeout = maps:get(timeout, Opts, ?DEFAULT_TIMEOUT),
    Ctx     = maps:get(context, Opts, #{}),
    Body    = render_body(maps:get(body, Opts, <<>>), Ctx),

    ensure_httpc(),

    Request  = build_request(Method, Url, Headers, Body),
    HttpOpts = [{timeout, Timeout}, {connect_timeout, Timeout}],
    ReqOpts  = [{body_format, binary}],

    case httpc:request(Method, Request, HttpOpts, ReqOpts, ?PROFILE) of
        {ok, {{_Vsn, Status, _Phrase}, RespHdrs, RespBody}} ->
            {ok, Status, headers_to_map(RespHdrs), RespBody};
        {error, _Reason} when AttemptsLeft > 1 ->
            Delay = backoff_delay(maps:get(retries, Opts, ?DEFAULT_RETRIES) - AttemptsLeft + 1),
            timer:sleep(Delay),
            call(Opts, AttemptsLeft - 1);
        {error, Reason} ->
            {error, Reason}
    end.

%% ---------------------------------------------------------------------------
%% Internal
%% ---------------------------------------------------------------------------

ensure_httpc() ->
    %% Start inets + ssl if not already running
    case application:ensure_started(inets) of
        ok             -> ok;
        {error, _}     -> ok
    end,
    case application:ensure_started(ssl) of
        ok          -> ok;
        {error, _}  -> ok
    end,
    %% Start (or reuse) a dedicated httpc profile — never touches the default.
    case inets:start(httpc, [{profile, ?PROFILE}]) of
        {ok, _}                       -> ok;
        {error, {already_started, _}} -> ok;
        _                             -> ok
    end,
    %% Configure proxy on our private profile only.
    %% Always exclude localhost so test callbacks work without a proxy.
    NoProxy = ["localhost", "127.0.0.1", "::1"],
    ProxyOpt = case proxy_from_env() of
        undefined       -> {proxy, {undefined, NoProxy}};
        {Host, Port}    -> {proxy, {{Host, Port}, NoProxy}}
    end,
    httpc:set_options([ProxyOpt], ?PROFILE).

proxy_from_env() ->
    Raw = case os:getenv("HTTPS_PROXY") of
        false -> os:getenv("HTTP_PROXY");
        V     -> V
    end,
    case Raw of
        false -> undefined;
        Url   ->
            case uri_string:parse(list_to_binary(Url)) of
                #{host := Host, port := Port} ->
                    {binary_to_list(Host), Port};
                #{host := Host} ->
                    {binary_to_list(Host), 8080};
                _ ->
                    undefined
            end
    end.

normalize_method(<<"GET">>)    -> get;
normalize_method(<<"POST">>)   -> post;
normalize_method(<<"PUT">>)    -> put;
normalize_method(<<"PATCH">>)  -> patch;
normalize_method(<<"DELETE">>) -> delete;
normalize_method(M) when is_binary(M) ->
    list_to_atom(string:lowercase(binary_to_list(M)));
normalize_method(M) when is_atom(M) -> M.

build_headers(Map) when is_map(Map) ->
    maps:fold(fun(K, V, Acc) ->
        [{binary_to_list(K), binary_to_list(V)} | Acc]
    end, [], Map);
build_headers(_) -> [].

%% GET/DELETE requests have no body in httpc's API
build_request(get,    Url, Headers, _Body) -> {Url, Headers};
build_request(delete, Url, Headers, _Body) -> {Url, Headers};
build_request(Method, Url, Headers, Body)
  when Method =:= post; Method =:= put; Method =:= patch ->
    ContentType  = proplists:get_value("content-type", Headers, "application/json"),
    CleanHeaders = proplists:delete("content-type", Headers),
    BodyBin = if is_binary(Body) -> Body; true -> <<>> end,
    {Url, CleanHeaders, ContentType, BodyBin}.

render_body(<<>>, _Ctx) -> <<>>;
render_body(Template, Ctx) when is_binary(Template), map_size(Ctx) =:= 0 ->
    Template;
render_body(Template, Ctx) when is_binary(Template) ->
    try bbmustache:render(Template, Ctx, [{key_type, binary}])
    catch _:_ -> Template
    end.

headers_to_map(Headers) when is_list(Headers) ->
    lists:foldl(fun({K, V}, Acc) ->
        maps:put(list_to_binary(K), list_to_binary(V), Acc)
    end, #{}, Headers);
headers_to_map(_) -> #{}.

backoff_delay(Attempt) ->
    trunc(math:pow(2, min(Attempt, 5))) * 1000.
