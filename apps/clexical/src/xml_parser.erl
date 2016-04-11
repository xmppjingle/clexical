-module(xml_parser).
-behavior(clexical_parser).

-include("../include/clexical.hrl").
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
	predicates_from_binary/1,
	to_binary/1	
	]).

excerpt_from_predicate(#predicate{abstract=Abstract}) ->
	Kin = exmpp_xml:get_child_elements(Abstract),
	lists:map(fun(Elem) -> predicate_from_elem(Elem) end, Kin).

predicates_from_binary(Bin) ->
	case ?Parse(Bin) of
		{xmlel,_,_,_,_,_} = Elem ->
			[predicate_from_elem(Elem)];
		_R ->
			[]
	end.

to_binary(#predicate{abstract=Elem}) ->
	exmpp_xml:document_to_binary(Elem).

predicate_from_elem({xmlel, _, _, Name, Attribs, _Children}=E) ->
	ID = exmpp_xml:get_attribute(E, <<"id">>, undefined),
	Subject = exmpp_xml:get_attribute(E, <<"subject">>, undefined),
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