-module(clexical_test).
-compile([export_all]).

-include("../include/clexical_test.hrl").

-define(CONFIG, [
    {clexical, [
        {redis_host, "localhost"},
        {redis_port, 6379},
        {runner, ?MODULE},
        {parser, xml_parser},
      	{hanger, ?MODULE}
    ]}
]).

setup_test_() ->
    ?meck_confetti([?CONFIG]),
    ?start_lager(),
    ?start_redo(),
    [Conf] = confetti:fetch(mgmt_conf),
    ClexiCfg = proplists:get_value(clexical, Conf, []),
    Parser = proplists:get_value(parser, ClexiCfg),
    Runner = proplists:get_value(runner, ClexiCfg),
    Hanger = proplists:get_value(hanger, ClexiCfg),
    {ok, _} = clexical:start_link(Parser, Runner, Hanger),

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
    Bin = <<"<offer id='1' subject='bestbuy' good='case'><onPurchase><celebrate/></onPurchase></offer>">>,
	gen_server:call(clexical, {submit, Bin}),
    timer:sleep(500),
	?_assert(true).

run(P) ->
	lager:debug("Test Runner: ~p~n", [P]),
	ok.

hang(K, V) ->
    lager:debug("Test Hang: ~p -> ~p~n", [K, V]),
    ok.

to_binary(_P) ->
    <<"data">>.