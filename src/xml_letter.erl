%%
%% xml_letter — XML ↔ #letter{} conversion.
%%
%% Uses OTP's built-in xmerl (no extra deps).
%%
%% Wire format
%% -----------
%%
%%   <letter subject="entity-id" author="system" type="decree" recipient="">
%%
%%     <!-- verb predicate: executed by the vassal -->
%%     <predicate action_type="verb" action="do:notify">
%%       <channel>slack</channel>
%%       <to>#ops</to>
%%       <message>Alert fired</message>
%%     </predicate>
%%
%%     <!-- preposition predicate: stored by the scribe (condition / context) -->
%%     <predicate action_type="preposition" action="if:fieldValue">
%%       <field>amount</field>
%%       <op>gt</op>
%%       <value>500</value>
%%     </predicate>
%%
%%     <!-- nested sub-letter via <abstract> -->
%%     <predicate action_type="verb" action="dispatch">
%%       <abstract>
%%         <letter subject="sub-entity" type="decree">
%%           <predicate action_type="verb" action="do:notify">
%%             <channel>ops</channel>
%%           </predicate>
%%         </letter>
%%       </abstract>
%%     </predicate>
%%
%%   </letter>
%%
%% Rules:
%%   • <letter> attributes: subject, author, recipient, type (decree|bulletin)
%%   • <predicate> attributes: action_type (verb|preposition), action, id, subject
%%   • Predicate adjectives are direct child elements: <key>value</key>
%%   • <abstract> child may contain a nested <letter> (recursively XML-encoded)
%%     or plain text (treated as opaque binary, e.g. JSON)
%%   • Unknown/empty elements are silently skipped
%%
-module(xml_letter).

-include_lib("xmerl/include/xmerl.hrl").
-include("../include/clexical.hrl").

-export([from_binary/1, to_binary/1]).

%% ---------------------------------------------------------------------------
%% Public API
%% ---------------------------------------------------------------------------

-spec from_binary(binary()) -> #letter{} | undefined.
from_binary(Bin) when is_binary(Bin) ->
    try
        {Doc, _} = xmerl_scan:string(binary_to_list(Bin), [{document, false}]),
        xml_to_letter(Doc)
    catch
        _:_ -> undefined
    end.

-spec to_binary(#letter{} | undefined) -> binary().
to_binary(#letter{} = L) ->
    Preds = [predicate_to_xml(P) || P <- L#letter.predicates],
    iolist_to_binary([
        <<"<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n">>,
        <<"<letter">>,
        xml_attr(<<"subject">>,   L#letter.subject),
        xml_attr(<<"author">>,    safe_bin(L#letter.author)),
        xml_attr(<<"recipient">>, safe_bin(L#letter.recipient)),
        xml_attr(<<"type">>,      atom_to_binary(L#letter.type, utf8)),
        <<">\n">>,
        Preds,
        <<"</letter>\n">>
    ]);
to_binary(_) ->
    <<"<letter/>\n">>.

%% ---------------------------------------------------------------------------
%% XML → record
%% ---------------------------------------------------------------------------

xml_to_letter(#xmlElement{name = letter, attributes = Attrs, content = Content}) ->
    #letter{
        subject    = attr_bin(Attrs, subject,   <<>>),
        author     = attr_bin(Attrs, author,    <<>>),
        recipient  = attr_bin(Attrs, recipient, <<>>),
        type       = to_letter_type(attr_bin(Attrs, type, <<"decree">>)),
        predicates = [xml_to_predicate(E)
                      || E <- Content, is_record(E, xmlElement),
                         E#xmlElement.name =:= predicate],
        via        = http
    };
xml_to_letter(_) ->
    undefined.

xml_to_predicate(#xmlElement{name = predicate,
                              attributes = Attrs,
                              content = Content}) ->
    Kind = to_action_type(attr_bin(Attrs, action_type, <<"verb">>)),
    Name = attr_bin(Attrs, action, <<>>),
    {Adjs, Abstract} = content_to_adjectives(Content),
    #predicate{
        id         = attr_bin(Attrs, id,      <<>>),
        subject    = attr_bin(Attrs, subject, <<>>),
        action     = {Kind, Name},
        adjectives = Adjs,
        abstract   = Abstract
    }.

%% Each child element of <predicate> becomes an adjective key→value pair.
%% The special <abstract> child is extracted separately.
content_to_adjectives(Elements) ->
    lists:foldl(fun
        (#xmlElement{name = abstract, content = Inner}, {Adjs, _}) ->
            Abstract = extract_abstract(Inner),
            {Adjs, Abstract};
        (#xmlElement{name = Key, content = ValContent}, {Adjs, Abstract}) ->
            Val = text_content(ValContent),
            {maps:put(atom_to_binary(Key, utf8), Val, Adjs), Abstract};
        (_, Acc) ->
            Acc
    end, {#{}, undefined}, Elements).

extract_abstract(Inner) ->
    %% If <abstract> contains a nested <letter>, encode it as JSON for
    %% the abstract field (the engine stores abstracts as JSON binaries).
    case [E || E <- Inner, is_record(E, xmlElement),
               E#xmlElement.name =:= letter] of
        [SubLetterEl] ->
            SubLetter = xml_to_letter(SubLetterEl),
            http_herald:to_binary(SubLetter);
        _ ->
            %% Plain text content (could be JSON or any opaque binary)
            case text_content(Inner) of
                <<>> -> undefined;
                Txt  -> Txt
            end
    end.

%% ---------------------------------------------------------------------------
%% record → XML
%% ---------------------------------------------------------------------------

predicate_to_xml(#predicate{action = {Kind, Name},
                             adjectives = Adjs,
                             abstract   = Abstract,
                             id         = Id,
                             subject    = Subj}) ->
    AdjElems = maps:fold(fun(K, V, Acc) ->
        VBin = val_to_bin(V),
        [<<"  <">>, K, <<">">>, xml_escape(VBin), <<"</">>, K, <<">\n">> | Acc]
    end, [], Adjs),
    AbstractElem = abstract_to_xml(Abstract),
    ExtraAttrs = case Id of
        <<>> -> [];
        _    -> [xml_attr(<<"id">>, Id)]
    end ++
    case Subj of
        <<>> -> [];
        _    -> [xml_attr(<<"subject">>, Subj)]
    end,
    [
        <<"<predicate">>,
        xml_attr(<<"action_type">>, atom_to_binary(Kind, utf8)),
        xml_attr(<<"action">>, Name),
        ExtraAttrs,
        <<">\n">>,
        AdjElems,
        AbstractElem,
        <<"</predicate>\n">>
    ].

abstract_to_xml(undefined) -> [];
abstract_to_xml(Bin) when is_binary(Bin) ->
    %% Try to decode the abstract as a JSON-encoded sub-letter and render it
    %% back as nested XML; fall back to CDATA text if parsing fails.
    case http_herald:letter_from_binary(Bin) of
        #letter{} = SubL ->
            [<<"  <abstract>\n">>,
             to_binary(SubL),
             <<"  </abstract>\n">>];
        _ ->
            [<<"  <abstract>">>, xml_escape(Bin), <<"</abstract>\n">>]
    end.

%% ---------------------------------------------------------------------------
%% Helpers
%% ---------------------------------------------------------------------------

attr_bin(Attrs, Name, Default) ->
    case lists:keyfind(Name, #xmlAttribute.name, Attrs) of
        #xmlAttribute{value = V} -> list_to_binary(V);
        false                    -> Default
    end.

text_content(Content) ->
    iolist_to_binary([T#xmlText.value
                      || T <- Content, is_record(T, xmlText)]).

xml_attr(_Name, <<>>) -> [];
xml_attr(_Name, undefined) -> [];
xml_attr(Name, Value) ->
    [<<" ">>, Name, <<"=\"">>, xml_escape(val_to_bin(Value)), <<"\"">>].

xml_escape(Bin) when is_binary(Bin) ->
    lists:foldl(fun({From, To}, Acc) ->
        binary:replace(Acc, From, To, [global])
    end, Bin, [
        {<<"&">>,  <<"&amp;">>},
        {<<"<">>,  <<"&lt;">>},
        {<<">">>,  <<"&gt;">>},
        {<<"\"">>, <<"&quot;">>},
        {<<"'">>,  <<"&apos;">>}
    ]);
xml_escape(Other) ->
    xml_escape(val_to_bin(Other)).

val_to_bin(V) when is_binary(V)  -> V;
val_to_bin(V) when is_atom(V)    -> atom_to_binary(V, utf8);
val_to_bin(V) when is_integer(V) -> integer_to_binary(V);
val_to_bin(V) when is_float(V)   -> float_to_binary(V, [{decimals, 6}]);
val_to_bin(V) when is_list(V)    ->
    case io_lib:printable_list(V) of
        true  -> list_to_binary(V);
        false -> list_to_binary(io_lib:format("~p", [V]))
    end;
val_to_bin(V)                    -> list_to_binary(io_lib:format("~p", [V])).

to_letter_type(<<"bulletin">>) -> bulletin;
to_letter_type(_)              -> decree.

to_action_type(<<"preposition">>) -> preposition;
to_action_type(_)                 -> verb.

safe_bin(undefined)              -> <<>>;
safe_bin(V) when is_binary(V)    -> V;
safe_bin(V) when is_atom(V)      -> atom_to_binary(V, utf8);
safe_bin(V) when is_list(V)      -> list_to_binary(V);
safe_bin(_)                      -> <<>>.
