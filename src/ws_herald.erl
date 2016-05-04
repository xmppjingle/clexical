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

start(Port) ->
	Dispatch = cowboy_router:compile([  
      {'_', [  
        {"/", cowboy_static, {priv_file, clexical, "index.html"}},
        {"/websocket", ws_herald, []}
      ]}  
    ]),  
    {ok, _} = cowboy:start_http(http, 100, [{port, Port}],  
        [{env, [{dispatch, Dispatch}]}]),
    mnesia_mind:init().

init({tcp, http}, _Req, _Opts) ->
	{upgrade, protocol, cowboy_websocket}.

% Herald Functions

proclaim(#predicate{action=Action}) -> 
	lager:info("Proclamation: ~p ~n", [Action]),
	ok.

read_excerpt(#predicate{}=P) ->
	xml_parser:excerpt_from_predicate(P).

curb(Seal, #predicate{}=P) ->
	lager:info("Curb: [~p] ~p~n", [Seal, P]),
	mnesia_mind:bear(Seal, P).

recall(Seal) ->
	lager:info("Recall: [~p] ~n", [Seal]),
	mnesia_mind:recollect(Seal).

% WebSocket Functions
websocket_init(_TransportName, Req, _Opts) ->
	{ok, Req, undefined_state}.

websocket_handle({text, Msg}, Req, State) ->
	Letter = xml_parser:letter_from_binary(Msg),
	lager:info("Received Letter: ~n ~p~n", [Letter]),
	case Letter of
		#letter{type = decree} -> 
			gen_server:call(clexical, {recite, Letter}),
			Reply = <<"<OK/>">>;
		#letter{type = bulletin} -> 
			gen_server:call(clexical, {attend, Letter}),
			Reply = <<"<OK/>">>;
		_ ->
			Reply = <<"<Error/>">>
	end,
	{reply, {text, Reply}, Req, State};
websocket_handle(_Data, Req, State) ->
	{ok, Req, State}.

websocket_info({timeout, _Ref, Msg}, Req, State) ->
	{reply, {text, Msg}, Req, State};
websocket_info(_Info, Req, State) ->
	{ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
	ok.