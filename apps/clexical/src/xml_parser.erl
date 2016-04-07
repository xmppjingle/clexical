-module(xml_parser).
-behaviour(clexical_parser).

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
	kin_from_predicate/1,
	predicates_from_binary/1,
	to_binary/1	
	]).

kin_from_predicate(#predicate{}) ->
	[].

predicates_from_binary(Bin) ->
	case ?Parse(Bin) of
		[Elem|_] ->
			[predicate_from_elem(Elem)];
		_ -> []
	end.

to_binary(#predicate{abstract=Elem}) ->
	exmpp_xml:document_to_binary(Elem).

predicate_from_elem({xmlel, _, _, Name, Attribs, _Children}=E) ->
	ID = exmpp_xml:get_attribute(E, <<"id">>, undefined),
	Subject = exmpp_xml:get_attribute(E, <<"subject">>, undefined),
	Adjectives = dict_from_attribs(Attribs),
	#predicate{id=ID, subject=Subject, action={verb, erlang:atom_to_binary(Name, ?ENCODE)}, adjectives=Adjectives, abstract=E}.

dict_from_attribs(Attribs) ->
	lists:foldl(fun({xmlattr, _, K, V}, Dict)-> dict:append(K, V, Dict) end, dict:new(), Attribs).
% 	dict_from_attribs_(Attribs, dict:new()).
% dict_from_attribs_([], Dict) ->
% 	Dict;
% dict_from_attribs_([{xmlattr, _, Name, Value}|T], Dict) ->
