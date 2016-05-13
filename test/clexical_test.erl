-module(clexical_test).
-behaviour(herald).
-behaviour(scribe).
-behaviour(vassal).

-export([
    proclaim/1,
    excerpt/1,
    curb/2,
    recall/1,
    work/2,
    letter_from_binary/1,
    to_binary/1,
    init/1
    ]).

-include("../include/clexical_test.hrl").

-define(CONFIG, [
    {clexical, [
        {herald, {?MODULE, [{port, 8082}]}},
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
    {ok, _} = clexical:start_link(Herald, Scribe, Vassal),

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
    News = #letter{predicates=[?PRED("1","set",{adverb, <<"purchased">>},[])]},
    lager:debug("News: ~p~n", [News]),
    Letter= #letter{predicates=[?PRED("1","set",{verb, <<"offer">>},[?PRED("1","set",{adverb, <<"purchased">>},[?PRED("1","set",{verb, <<"celleb">>},[])])])]},
	gen_server:call(clexical, {recite, Letter}),
    timer:sleep(500),
    gen_server:call(clexical, {attend, News}),
	?_assert(true).

proclaim(L) ->
	lager:debug("Test Proclaim: ~p~n", [L]),
	ok.

curb(K, V) ->
    lager:debug("Test Curb: ~p -> ~p~n", [K, V]),
    ok.

recall(K) ->
    V=?PRED("1","set",{adverb, purchased},[?PRED("1","set",{verb, <<"celleb">>},[])]),
    lager:debug("Test Recall: ~p -> ~p~n", [K, V]),
    V.

excerpt(#predicate{abstract=E}) ->
    lager:debug("Test Read Excerpt: ~p ~n", [E]),
    #letter{predicates=E}.

work(#predicate{}=P, _)->
    lager:debug("Test Work: ~p ~n", [P]),
    ok.    

letter_from_binary(_) ->
    undefined.

to_binary(_) ->
    undefined.

init(_Opts) ->
    ok.