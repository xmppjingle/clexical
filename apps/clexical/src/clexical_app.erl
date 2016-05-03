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
    RedisOpts = redis_opts(Config),
    Herald = proplists:get_value(herald, Config),
    clexical_sup:start_link([[Herald], [RedisOpts]]).

stop(_State) ->
    ok.

redis_opts(Config) ->
    Host = proplists:get_value(redis_host, Config),
    Port = proplists:get_value(redis_port, Config),    
    case {Host, Port} of
        {undefined, _} -> undefined;
        {_, undefined} -> undefined;
        {Host, Port} -> [{host, Host}, {port, Port}]
    end.