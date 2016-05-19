-module(xml_mnesia_scribe_test).

-compile([export_all]).

-include("../include/clexical_test.hrl").

setup_test_() ->
    mnesia_scribe:init([]),
    ?start_lager(),
    {setup,
        spawn,
        fun init_per_suite/0,
        fun end_per_suite/1,
        [        
        ]
    }.

init_per_suite() ->
    ok.

end_per_suite(_Config) ->
    ok.

basic_bear_in_mind_test() ->
	P = ?PRED("1","set",{verb, <<"offer">>},[?PRED("1","set",{preposition, <<"purchased">>},[?PRED("1","set",{verb, celleb},[])])]),
	K = clexical:compose_key(P),
	mnesia_scribe:curb(K, P),
	PP = mnesia_scribe:recall(K),
    ?assertEqual(P, PP).
