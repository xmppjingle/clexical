-module(ws_herald).
-behaviour(cowboy_websocket_handler).
-behaviour(herald).

-include("../include/clexical.hrl").

-define(Clean(Stanza), begin
  case Stanza of
      undefined ->
        undefined;
      _ ->  
        exmpp_xml:remove_whitespaces_deeply(Stanza)
  end
end).

-define(Parse(XML), begin
    exmpp_xml:remove_whitespaces_deeply(lists:nth(1, exmpp_xml:parse_document(XML,[{names_as_atom, true}])))  
end). 

% -record(ws_state, {reactive_vassal}).

-export([
	init/3,
    websocket_init/3, websocket_handle/3,
    websocket_info/3, websocket_terminate/3
]).

-export([
	init/1,
    proclaim/1,
	excerpts/1,
	letter_from_binary/1,
	to_binary/1,
	predicate_from_binary/1
    ]).

% Herald Functions
init(Opts) ->
	Dispatch = cowboy_router:compile([  
      {'_', [  
        {"/", cowboy_static, {file, priv_dir(clexical), "index.html"}},
        {"/websocket", ws_herald, []}
      ]}  
    ]),  
    {ok, _} = cowboy:start_http(http, 100, [{port, clexical:get_option(port, Opts, 8084)}],  
        [{env, [{dispatch, Dispatch}]}]).

proclaim(#letter{author=Author}=L) -> 
	lager:info("Proclamation: ~p -> ~p~n", [L, Author]),
	case erlang:is_pid(Author) of
		true ->
			Bin = to_binary(L),
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
	L = letter_from_binary(Msg),
	Letter = L#letter{author=self()},
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

% Utils 
priv_dir(App) ->
    case code:priv_dir(App) of
        {error, bad_name} ->
            {ok, Cwd} = file:get_cwd(),
            Cwd ++ "/" ++ "priv";
        Priv ->
            Priv
    end.

-spec excerpts(#predicate{}) -> [#predicate{}]|[].
excerpts(#predicate{abstract=undefined}) ->
	[];
excerpts(#predicate{abstract=Abstract, author=Author}) ->
	Kin = exmpp_xml:get_child_elements(Abstract),
	lists:map(fun(Elem) -> (predicate_from_elem(Elem))#predicate{author=Author} end, Kin);
excerpts(_) ->
	[].

letter_from_binary(Bin) ->
	case ?Parse(Bin) of
		{xmlel,_,_,Type,_,Children} = Elem ->
			P = lists:map(fun(E) -> predicate_from_elem(E) end, Children),
			#letter{type=Type, predicates=P, author=exmpp_xml:get_attribute(Elem, <<"author">>, <<>>)};
		_R ->
			lager:info("Invalid Letter Type: ~p ~n", [_R]),
			undefined
	end.

to_binary(#letter{predicates=[], type=Type}) ->
	BType = erlang:list_to_binary(erlang:atom_to_list(Type)),
	<<"<", BType/binary, "/>">>;
to_binary(#letter{predicates=PS, type=Type}) ->
	PP = to_binary_(PS),
	BType = erlang:list_to_binary(erlang:atom_to_list(Type)),
	<<"<", BType/binary, ">", PP/binary, "</", BType/binary, ">">>;
to_binary(_) ->
	<<>>.

to_binary_([]) ->
	<<>>;
to_binary_([#predicate{abstract=Elem}|T])->
	P = exmpp_xml:document_to_binary(Elem),
	PP = to_binary_(T),
	<<P/binary, PP/binary>>.

predicate_from_binary(Bin) ->
	case ?Parse(Bin) of
		{xmlel,_,_,_Type,_,_Children} = Elem ->
			predicate_from_elem(Elem);
		_R ->
			lager:info("Invalid Predicate Type: ~p ~n", [_R]),
			undefined
	end.	

predicate_from_elem({xmlel, _, _, Name, Attribs, _Children}=E) ->
	ID = exmpp_xml:get_attribute(E, <<"id">>, <<>>),
	Subject = exmpp_xml:get_attribute(E, <<"subject">>, <<>>),
	Adjectives = dict_from_attribs(Attribs),
	ActionName = erlang:atom_to_binary(Name, ?ENCODE),
	#predicate{id=ID, subject=Subject, action={get_kind(ActionName), ActionName}, adjectives=Adjectives, abstract=E}.

dict_from_attribs(Attribs) ->
	lists:foldl(fun({xmlattr, _, K, V}, Dict)-> case K of <<"id">> -> Dict; <<"subject">> -> Dict; _ -> dict:append(K, V, Dict) end end, dict:new(), Attribs).

get_kind(Name) ->
    case Name of
        <<"on",_/binary>> ->
            preposition;
        _ ->
            verb
    end.
