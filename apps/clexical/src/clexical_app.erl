-module(clexical_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    init_cowboy(),
    clexical_sup:start_link([]).

stop(_State) ->
    ok.

init_cowboy() ->
    Dispatch =
        cowboy_router:compile([{
            '_', % Host
            [
            {"/clexical", clexical, []}
            ] % Paths
        }]),
    Port = 9999,
    Listeners = 10,
    Timeout = 500,
    {ok, _} = cowboy:start_http(http, Listeners, [{port, Port}],
        [{env, [{dispatch, Dispatch}]}, {timeout, Timeout}]).
