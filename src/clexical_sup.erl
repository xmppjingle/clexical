-module(clexical_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Args), {I, {I, start_link, Args}, permanent, 5000, worker, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link([Herald, Scribe, Vassal]) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Herald, Scribe, Vassal]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([Herald, Scribe, Vassal]) ->
    {ok, {{one_for_one, 5, 10}, [
    	?CHILD(clexical, [Herald, Scribe, Vassal])
    ]}}.