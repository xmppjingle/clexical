-module(xml_mnesia_scribe_test).

-compile([export_all]).

-include("../include/clexical_test.hrl").

setup_test_() ->
    clexical_id:start_link([]),
    clexical:start_link({xmpp_herald, []}, {mnesia_scribe, []}, {?MODULE, []}),
    ?start_lager(),
    ?start_apps(),
    {setup,
        spawn,
        fun init_per_suite/0,
        fun end_per_suite/1,
        [        
        ]
    }.

initialize(_) ->
    ok.

init_per_suite() ->
    ok.

end_per_suite(_Config) ->
    clexical:terminate(ok, ok),
    ok.

basic_bear_in_mind_test() ->
	#letter{predicates = [P|_]} = xmpp_herald:letter_from_binary(<<"<iq id='", ?ANY_SUBJECT/binary, "'><onOffer id='", ?ANY_ID/binary, "'><test1/><test2/></onOffer></iq>">>),
    lager:debug("P: ~p ~n", [P]),
	K = clexical:compose_key(P),
	mnesia_scribe:curb(K, P),
	PP = mnesia_scribe:recall(K),
    ?assertEqual(P, PP).

process_test() ->
    xmpp_herald:process_letter(xmpp_herald:letter_from_binary(<<"<iq id='", ?ANY_SUBJECT/binary, "'><onOffer id='", ?ANY_ID/binary, "'><test1/><test2/></onOffer></iq>">>)),
    timer:sleep(200),
    xmpp_herald:process_letter(xmpp_herald:letter_from_binary(<<"<presence id='", ?ANY_SUBJECT/binary, "'><onOffer id='", ?ANY_ID/binary, "'/></presence>">>)),   
    ?assertEqual(1, 1).

merge_adjectives_test() ->
    #letter{predicates = [P|_]} = xmpp_herald:letter_from_binary(<<"<iq id='", ?ANY_SUBJECT/binary, "'><onOffer id='", ?ANY_ID/binary, "'><test1 a='a'/><test2 b='b'/></onOffer></iq>">>),
    M = xmpp_herald:merge_adjectives(P),
    lager:debug("M: ~p ~n", [M]),
    ?assertEqual(#{<<"a">> => <<"a">>,<<"b">> => <<"b">>,<<"id">> => <<"*ID*">>}, M).

merge_adjectives_letters_test() ->
    #letter{predicates = [P|_]} = xmpp_herald:letter_from_binary(<<"<iq id='", ?ANY_SUBJECT/binary, "'><onOffer id='", ?ANY_ID/binary, "'><test1 a='a'/><test2 b='b'/></onOffer></iq>">>),
    #letter{predicates = [PP|_]} = xmpp_herald:letter_from_binary(<<"<iq id='", ?ANY_SUBJECT/binary, "'><onOffer id='", ?ANY_ID/binary, "'><test1 c='c'/><test2 d='d'/></onOffer></iq>">>),
    MM = xmpp_herald:merge_adjectives(P, PP),
    lager:debug("MM: ~p ~n", [MM]),
    ?assertEqual(MM, #{<<"a">> => <<"a">>,<<"b">> => <<"b">>,<<"c">> => <<"c">>,<<"d">> => <<"d">>,<<"id">> => <<"*ID*">>}).

work(_L, _LP) ->
    lager:debug("Test Work: ~p on: ~p ~n", [_L, _LP]),
    ok.