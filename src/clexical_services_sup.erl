%%
%% clexical_services_sup — supervisor for pluggable Herald and Vassal processes.
%%
%% Both must export start_link/1 accepting their Opts map.
%% This keeps them under the OTP supervision tree so crashes are
%% automatically recovered — essential for permanent automations.
%%

-module(clexical_services_sup).
-behaviour(supervisor).

-export([start_link/2]).
-export([init/1]).

start_link(HeraldSpec, VassalSpec) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, {HeraldSpec, VassalSpec}).

init({{Herald, HOpts}, {Vassal, VOpts}}) ->
    Children = [
        service_child(Herald, HOpts),
        service_child(Vassal, VOpts)
    ],
    {ok, {{one_for_one, 5, 10}, Children}}.

service_child(Module, Opts) ->
    {Module, {Module, start_link, [Opts]}, permanent, 5000, worker, [Module]}.
