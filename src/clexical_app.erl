-module(clexical_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    Herald = application:get_env('clexical', herald, []),
    Scribe = application:get_env('clexical', scribe, []),
    Vassal = application:get_env('clexical', vassal, []),
    clexical_sup:start_link([Herald, Scribe, Vassal]).

stop(_State) ->
    ok.