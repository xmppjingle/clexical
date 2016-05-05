-module(clexical_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    [Conf] = confetti:fetch(mgmt_conf),
    Config = proplists:get_value(clexical, Conf, []),
    Herald = proplists:get_value(herald, Config),
    Scribe = proplists:get_value(scribe, Config),
    Vassal = proplists:get_value(vassal, Config),
    clexical_sup:start_link([Herald, Scribe, Vassal]).

stop(_State) ->
    ok.