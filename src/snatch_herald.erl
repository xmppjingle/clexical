-module(snatch_herald).
-include("../include/clexical.hrl").

-export([
	init/1,
    proclaim/1,
	excerpts/1,
	letter_from_binary/1,
	to_binary/1,
	predicate_from_binary/1,
	process_letter/1
	]).

-spec init(Opts::any()) -> any().
init(Opts) ->
	application:start(fast_xml),
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
	case fxml_stream:parse_element(Bin) of
		#xmlel{children = Children, attrs = Attrs} = Elem ->
			P = lists:map(fun(E) -> predicate_from_elem(E) end, Children),
			#letter{type=Type, predicates=P, author=fxml:get_attribute(<<"author">>, Attrs)};
		_R ->
			lager:info("Invalid Letter Type: ~p ~n", [_R]),
			undefined
	end.

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

predicate_from_elem(#xmlel{name = Name, attrs = Attribs}=E) ->
	ID = exmpp_xml:get_attribute(E, <<"id">>, <<>>),
	Subject = exmpp_xml:get_attribute(E, <<"subject">>, <<>>),
	Adjectives = maps:from_list(Attribs),
	ActionName = erlang:atom_to_binary(Name, ?ENCODE),
	#predicate{id=ID, subject=Subject, action={get_kind(ActionName), ActionName}, adjectives=Adjectives, abstract=E}.