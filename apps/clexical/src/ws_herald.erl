-module(ws_herald).
-behaviour(cowboy_websocket_handler).
-behaviour(herald).

-include("../include/clexical.hrl").

-export([start/0]).
-export([
	init/3,
    websocket_init/3, websocket_handle/3,
    websocket_info/3, websocket_terminate/3
]).

-export([
    proclaim/1,
    read_excerpt/1,
    curb/2,
    recall/1
    ]).

start() ->
	Dispatch = cowboy_router:compile([  
      {'_', [  
        {"/", cowboy_static, {priv_file, clexical, "index.html"}},
        {"/websocket", ws_herald, []}  
      ]}  
    ]),  
    {ok, _} = cowboy:start_http(http, 100, [{port, 8081}],  
        [{env, [{dispatch, Dispatch}]}]).

init({tcp, http}, _Req, _Opts) ->
	{upgrade, protocol, cowboy_websocket}.


% Herald Functions

proclaim(#predicate{action=Action}) -> 
	lager:info("Proclamation: ~p ~n", [Action]),
	ok.

read_excerpt(#predicate{}=P) ->
	xml_parser:excerpt_from_predicate(P).

curb(Seal, #predicate{}=P) ->
	mnesia_mind:bear(Seal, P).

recall(Seal) ->
	mnesia_mind:recollect(Seal).

% WebSocket Functions
websocket_init(_TransportName, Req, _Opts) ->
	{ok, Req, undefined_state}.

websocket_handle({text, Msg}, Req, State) ->
	PS = xml_parser:predicates_from_binary(Msg),
	lager:info("Received Predicates: ~n ~p~n", [PS]),
	gen_server:call(clexical, {recite, #letter{predicates=PS}}),
	{reply, {text, <<"<OK/>">>}, Req, State};
websocket_handle(_Data, Req, State) ->
	{ok, Req, State}.

websocket_info({timeout, _Ref, Msg}, Req, State) ->
	{reply, {text, Msg}, Req, State};
websocket_info(_Info, Req, State) ->
	{ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
	ok.