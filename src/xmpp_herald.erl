-module(xmpp_herald).
-include("../include/clexical.hrl").
-include_lib("xmpp.hrl").

-export([
	init/1,
    proclaim/1,
	excerpts/1,
	letter_from_binary/1,
	to_binary/1,
	predicate_from_binary/1,
	process_letter/1
	]).

-export([
	get_kind/1,
	get_type/1,
	letter_from_xmlel/1
	]).

-define(OK, <<"<ok/>">>).
-define(ERROR, <<"<error/>">>).

-spec init(Opts::any()) -> any().
init(_Opts) ->
	application:start(xmpp),
	ok.

-spec proclaim(#letter{}) -> ok|error.
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

-spec letter_from_binary(Binary :: binary()) -> undefined|#letter{}.
letter_from_binary(Bin) ->
	letter_from_xmlel( fxml_stream:parse_element(Bin) ).

-spec letter_from_xmlel(#xmlel{}) -> undefined|#letter{}.
letter_from_xmlel(#xmlel{name = Type, children = Children, attrs = Attrs}) ->
	P = lists:map(fun(E) -> predicate_from_elem(E) end, Children),
	#letter{type = get_type(Type), predicates=P, author=fxml:get_attr(<<"from">>, Attrs)};
letter_from_xmlel(_R) -> 
	lager:info("Invalid Letter Type: ~p ~n", [_R]),
	undefined.

-spec to_binary(#letter{}) -> undefined|binary().
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
	P = fxml_xml:element_to_binary(Elem),
	PP = to_binary_(T),
	<<P/binary, PP/binary>>.

-spec excerpts(#predicate{}) -> []|[#predicate{}].
excerpts(#predicate{abstract=undefined}) ->
	[];
excerpts(#predicate{abstract=#xmlel{children = Kin}, author=Author}) ->
	lists:map(fun(Elem) -> (predicate_from_elem(Elem))#predicate{author=Author} end, Kin);
excerpts(_) ->
	[].

predicate_from_binary(Bin) ->
	case fxml_stream:parse_element(Bin) of
		#xmlel{} = Elem ->
			predicate_from_elem(Elem);
		_R ->
			lager:info("Invalid Predicate Type: ~p ~n", [_R]),
			undefined
	end.	

predicate_from_elem(#xmlel{name = ActionName, attrs = Attribs}=E) ->
	ID = fxml:get_attr(<<"id">>, Attribs),
	Subject = fxml:get_attr(<<"subject">>, Attribs),
	Adjectives = maps:from_list(Attribs),
	#predicate{id=ID, subject=Subject, action={get_kind(ActionName), ActionName}, adjectives=Adjectives, abstract=E}.

get_type(<<"iq">>) ->
	decree;
get_type(<<"presence">>) ->
	bulletin.

get_kind(<<"on",_/binary>>) ->
	preposition;
get_kind(_) ->
    verb.

process_letter(Letter) ->
	case Letter of
		#letter{type = decree} -> 
			clexical:recite(Letter),
			?OK;
		#letter{type = bulletin} -> 
			clexical:attend(Letter),
			?OK;
		_ ->
			?ERROR
	end.