-module(clexical_test).
-behaviour(herold).

-export([
    proclaim/1,
    read_excerpt/1,
    curb/2
    ]).

-include("../include/clexical_test.hrl").

-define(CONFIG, [
    {clexical, [
        {redis_host, "localhost"},
        {redis_port, 6379},
        {herald, ?MODULE}
        ]}
]).

setup_test_() ->
    ?meck_confetti([?CONFIG]),
    ?start_lager(),
    ?start_redo(),
    [Conf] = confetti:fetch(mgmt_conf),
    ClexiCfg = proplists:get_value(clexical, Conf, []),
    Herald = proplists:get_value(herald, ClexiCfg),
    {ok, _} = clexical:start_link(Herald),

    {setup,
        spawn,
        fun init_per_suite/0,
        fun end_per_suite/1,
        fun(_Config) -> {timeout, 4, ?_test(single_execution())} end
    }.

init_per_suite() ->
    random:seed(erlang:now()),    
    ok.

end_per_suite(_Config) ->
    meck:unload(),
    ok.

single_execution() ->
    Letter= #letter{predicates=[#predicate{id= <<"1">>, subject= <<"set">>, abstract=[#predicate{id= <<"1">>, subject= <<"set">>, abstract=[], action={adverb, purchased}}], action={verb, offer}}]},
	gen_server:call(clexical, {recite, Letter}),
    timer:sleep(500),
	?_assert(true).

proclaim(P) ->
	lager:debug("Test proclaim: ~p~n", [P]),
	ok.

curb(K, V) ->
    lager:debug("Test remark: ~p -> ~p~n", [K, V]),
    ok.

read_excerpt(#predicate{abstract=E}) ->
    lager:debug("Test Read Excerpt: ~p ~n", [E]),
    #letter{predicates=E}.
