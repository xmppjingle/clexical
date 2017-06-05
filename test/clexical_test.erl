-module(clexical_test).
-behaviour(herald).
-behaviour(scribe).
-behaviour(vassal).

-export([
    proclaim/1,
    excerpts/1,
    curb/2,
    recall/1,
    work/2,
    letter_from_binary/1,
    to_binary/1,
    initialize/1
    ]).

-include("../include/clexical_test.hrl").

-define(CONFIG, [
    {clexical, [
        {herald, {?MODULE, [{port, 8084}]}},
        {scribe, {?MODULE, []}},
        {vassal, {?MODULE, []}}
        ]}
]).

setup_test_() ->
    ?meck_confetti([?CONFIG]),
    ?start_lager(),
    [Conf] = confetti:fetch(mgmt_conf),
    ClexiCfg = proplists:get_value(clexical, Conf, []),
    Herald = proplists:get_value(herald, ClexiCfg),
    Scribe = proplists:get_value(scribe, ClexiCfg),
    Vassal = proplists:get_value(vassal, ClexiCfg),
    % {ok, _} = clexical:start_link({clexical_test, Herald}, {clexical_test, Scribe}, {clexical_test, Vassal}),

    {setup,
        spawn,
        fun init_per_suite/0,
        fun end_per_suite/1,
        fun(_Config) -> {timeout, 4, ?_test(single_execution())} end
    }.

init_per_suite() ->
    ok.

end_per_suite(_Config) ->
    meck:unload(),
    clexical:terminate(ok, ok),
    ok.

single_execution() ->
	?_assert(true).

proclaim(L) ->
	lager:debug("Test Proclaim: ~p~n", [L]),
	ok.

curb(K, V) ->
    lager:debug("Test Curb: ~p -> ~p~n", [K, V]),
    ok.

recall(K) ->
    V = <<>>,
    lager:debug("Test Recall: ~p -> ~p~n", [K, V]),
    V.

excerpts(#predicate{abstract=E}) ->
    lager:debug("Test Read Excerpt: ~p ~n", [E]),
    E.

work(#letter{}=L, _)->
    lager:debug("Test Work: ~p ~n", [L]),
    ok.    

letter_from_binary(_) ->
    undefined.

to_binary(_) ->
    undefined.

initialize(_Opts) ->
    ok.