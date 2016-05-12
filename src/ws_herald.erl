-module(ws_herald).
-behaviour(cowboy_websocket_handler).
-behaviour(herald).

-include("../include/clexical.hrl").

-export([
	init/3,
    websocket_init/3, websocket_handle/3,
    websocket_info/3, websocket_terminate/3
]).

-export([
    proclaim/1,
    init/1
    ]).

% Herald Functions
init(Opts) ->
	Dispatch = cowboy_router:compile([  
      {'_', [  
        {"/", cowboy_static, {file, utils:priv_dir(clexical), "index.html"}},
        {"/websocket", ws_herald, []}
      ]}  
    ]),  
    {ok, _} = cowboy:start_http(http, 100, [{port, clexical:get_option(port, Opts, 8084)}],  
        [{env, [{dispatch, Dispatch}]}]).

proclaim(#letter{author=Author}=L) -> 
	lager:info("Proclamation: ~p -> ~p~n", [L, Author]),
	case erlang:is_pid(Author) of
		true ->
			Bin = xml_mnesia_scribe:to_binary(L),
			lager:info("Proclamation Data: ~p ~n", [Bin]),
			Author ! {send, Bin};
		_ ->
			lager:debug("No Destination: ~p ~n", [L]),
			ok
	end.

% WebSocket Functions
init({tcp, http}, _Req, _Opts) ->
	{upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
	{ok, Req, undefined_state}.

websocket_handle({text, Msg}, Req, State) ->
	L = xml_mnesia_scribe:letter_from_binary(Msg),
	Letter = L#letter{author=self()},
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

websocket_info({send, Payload}, Req, State) ->
	{reply, {text, Payload}, Req, State};
websocket_info(_Info, Req, State) ->
	{ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
	ok.

