-module(clexical_sup).
-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).

-define(CHILD(I, Args), {I, {I, start_link, Args}, permanent, 5000, worker, [I]}).

start_link([Herald, Scribe, Vassal]) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Herald, Scribe, Vassal]).

init([Herald, Scribe, Vassal]) ->
    Children = [
        ?CHILD(clexical_id,       [[]]),
        ?CHILD(clexical_notifier, []),
        ?CHILD(clexical,          [Herald, Scribe, Vassal])
    ],
    {ok, {{one_for_one, 5, 10}, Children}}.
