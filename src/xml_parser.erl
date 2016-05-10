-module(xml_parser).
-behavior(clexical_parser).

-include("../include/clexical.hrl").
-include("../include/clexical_app.hrl").
-include_lib("exmpp/include/exmpp.hrl").

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

%% API
-export([
	excerpt_from_predicate/1,
	letter_from_binary/1,
	to_binary/1	
	]).

excerpt_from_predicate(undefined) ->
	#letter{predicates=[]};
excerpt_from_predicate(#predicate{abstract=undefined}) ->
	#letter{predicates=[]};
excerpt_from_predicate(#predicate{abstract=Abstract}) ->
	Kin = exmpp_xml:get_child_elements(Abstract),
	P = lists:map(fun(Elem) -> predicate_from_elem(Elem) end, Kin),
	#letter{predicates=P}.

letter_from_binary(Bin) ->
	case ?Parse(Bin) of
		{xmlel,_,_,Type,_,Children} = Elem ->
			P = lists:map(fun(E) -> predicate_from_elem(E) end, Children),
			#letter{type=Type, predicates=P, sender=exmpp_xml:get_attribute(Elem, <<"sender">>, <<"anonymous">>)};
		_R ->
			lager:info("Invalid Letter Type: ~p ~n", [_R]),
			undefined
	end.

to_binary(#letter{predicates=[#predicate{abstract=Elem}|T]}=Letter) ->
	P = exmpp_xml:document_to_binary(Elem),
	PP = to_binary(Letter#letter{predicates=T}),
	<<P/binary, PP/binary>>;
to_binary(_) ->
	<<>>.

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
            adverb;
        _ ->
            verb
    end.