%%
%% clexical_sup — root supervisor.
%%
%% Startup order:
%%   1. clexical_id            — unique ID generator
%%   2. clexical_notifier      — pub-sub for proclaim results
%%   3. clexical_services_sup  — sub-supervisor owning Herald + Vassal processes
%%   4. clexical               — the core evaluator engine
%%
%% Herald and Vassal are owned by clexical_services_sup so that crashes
%% (HTTP listener down, automation timer loop) are restarted automatically
%% without touching the evaluator.  Scribe (Mnesia) is stateless so it is
%% just initialised inline from clexical:init/1.
%%

-module(clexical_sup).
-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).

start_link([HeraldSpec, ScribeSpec, VassalSpec]) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE,
                          {HeraldSpec, ScribeSpec, VassalSpec}).

init({{Herald, _HOpts} = HSpec,
     {Scribe, SOpts},
     {Vassal, _VOpts}  = VSpec}) ->
    Children = [
        worker(clexical_id,      [[]]),
        worker(clexical_notifier,[]),
        {clexical_services_sup,
         {clexical_services_sup, start_link, [HSpec, VSpec]},
         permanent, infinity, supervisor, [clexical_services_sup]},
        {clexical,
         {clexical, start_link, [Herald, {Scribe, SOpts}, Vassal]},
         permanent, 5000, worker, [clexical]}
    ],
    {ok, {{one_for_one, 5, 10}, Children}}.

worker(Module, Args) ->
    {Module, {Module, start_link, Args}, permanent, 5000, worker, [Module]}.
