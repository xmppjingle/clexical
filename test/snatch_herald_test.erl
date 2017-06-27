-module(snatch_herald_test).

-compile([export_all]).

-include("../include/clexical_test.hrl").

setup_test_() ->
    application:start(xmpp),
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
    meck:unload(),
    ok.

basic_parse_test() ->
    Bin = <<"<decree><offer id='1' subject='bestbuy' good='case'><onPurchase><celebrate/></onPurchase></offer></decree>">>,
    L = xmpp_herald:letter_from_binary(Bin),
    ?assert(L /= undefined),
    #letter{predicates=[#predicate{adjectives=Adjectives}|_]}=L,
    ?assert(<<"case">> == clexical:get_adjective(<<"good">>, Adjectives)),
    ?assert(undefined == clexical:get_adjective(<<"goodDeeds">>, Adjectives)),
    [P|_] = L#letter.predicates, 
    K = xmpp_herald:excerpts(P),
    ?assert(K /= undefined).

basic_cleanup_test() ->
    Bin = <<"<decree>
            <offer id='1' subject='bestbuy' good='case'>
                <onPurchase>
                            <celebrate/>
                </onPurchase>
            </offer>
            </decree>">>,
    X = fxml_stream:parse_element(Bin),
    CX = clexical_utils:remove_whitespaces_deeply(X),
    lager:debug("Dirty XML: ~p ~n", [X]),
    lager:debug("Clean XML: ~p ~n", [CX]),
    ?assertEqual(
        {xmlel,<<"decree">>,[],[{xmlel,<<"offer">>,[{<<"id">>,<<"1">>},{<<"subject">>,<<"bestbuy">>},{<<"good">>,<<"case">>}],[{xmlel,<<"onPurchase">>,[],[{xmlel,<<"celebrate">>,[],[]}]}]}]},
        CX).