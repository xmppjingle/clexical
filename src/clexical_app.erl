-module(clexical_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    %% Config is a proplist:
    %%   [{herald, {Module, OptsMap}},
    %%    {scribe, {Module, OptsMap}},
    %%    {vassal, {Module, OptsMap}}]
    %%
    %% Resolved in priority order:
    %%   1. application env  (sys.config)
    %%   2. OS env vars      (CLEXICAL_* vars)
    Config  = application:get_env(clexical, config, default_config()),
    Herald  = proplists:get_value(herald, Config),
    Scribe  = proplists:get_value(scribe, Config),
    Vassal  = proplists:get_value(vassal, Config),
    clexical_sup:start_link([Herald, Scribe, Vassal]).

stop(_State) ->
    ok.

%% ---------------------------------------------------------------------------
%% Default: HTTP herald + Mnesia scribe + Automation vassal
%% ---------------------------------------------------------------------------
default_config() ->
    Port   = env_int("CLEXICAL_HTTP_PORT", 8080),
    ApiKey = os:getenv("CLEXICAL_API_KEY", "changeme"),
    [
        {herald, {http_herald,       #{port => Port, api_key => ApiKey}}},
        {scribe, {mnesia_scribe,     #{}}},
        {vassal, {automation_vassal, #{}}}
    ].

env_int(Var, Default) ->
    case os:getenv(Var) of
        false -> Default;
        S     -> list_to_integer(S)
    end.
