%%
%% xml_letter — XML ↔ #letter{} conversion.
%%
%% Uses OTP's built-in xmerl (no extra deps).
%%
%% Two wire formats are accepted simultaneously in the same document:
%%
%% 1. Verbose <predicate> form
%% ---------------------------
%%
%%   <letter subject="entity-id" author="system" type="decree" recipient="">
%%
%%     <!-- verb predicate -->
%%     <predicate action_type="verb" action="notify">
%%       <channel>slack</channel>
%%       <to>#ops</to>
%%     </predicate>
%%
%%     <!-- preposition predicate (event trigger) -->
%%     <predicate action_type="preposition" action="on:create">
%%       <entity>order</entity>
%%     </predicate>
%%
%%   </letter>
%%
%% 2. DSL shorthand form (element name = action; attributes = adjectives)
%% -----------------------------------------------------------------------
%%
%% Convention — only two cases:
%%
%%   on:*   (starts with "on") → event / trigger  → preposition
%%   <anything else>           → action            → verb
%%
%%   <letter subject="order-99" author="checkout" type="decree">
%%     <on:create  entity="order" source="web"/>          <!-- event  -->
%%     <transitionStatus to="fraud_review"/>              <!-- action -->
%%     <notify channel="slack" to="#fraud-team"/>         <!-- action -->
%%     <webhook url="https://risk.internal/score"
%%              method="POST" timeout="4000"/>            <!-- action -->
%%   </letter>
%%
%% Both forms can be mixed freely in the same <letter>.
%%
%% Rules (DSL form):
%%   • Name starts with "on" → {preposition, Name}  (event trigger)
%%   • Anything else         → {verb,        Name}  (action)
%%   • `id` and `subject` attributes → predicate.id / predicate.subject
%%   • All other attributes  → adjectives map
%%   • <abstract> is not supported in DSL shorthand (use verbose form for that)
%%
%% Event attribute semantics (on:* predicates)
%% -------------------------------------------
%%
%%   Attributes without "_" prefix → pattern match constraint:
%%     <on:complete status_code="200"/>
%%     fires only when the incoming event has status_code = "200"
%%
%%   Attributes with "_" prefix → binding (capture variable):
%%     <on:complete _status_code="result_code"/>
%%     fires for ANY status_code value; binds the received value to
%%     the variable "result_code" for use downstream in the letter
%%
%%   Mixed:
%%     <on:complete status_code="200" _body="response_body"/>
%%     fires only when status_code = "200"; captures body → response_body
%%
%%   Use event_adjectives/1 to split a predicate's adjectives into
%%   {Matches, Bindings} maps.
%%
%% Output (to_binary/1) always uses the verbose <predicate> form.
%%
-module(xml_letter).

-include_lib("xmerl/include/xmerl.hrl").
-include("../include/clexical.hrl").

-export([from_binary/1, to_binary/1, event_adjectives/1]).

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
        predicates = lists:filtermap(fun(E) ->
                         case is_record(E, xmlElement) of
                             true ->
                                 case xml_to_predicate(E) of
                                     #predicate{} = P -> {true, P};
                                     _                -> false
                                 end;
                             false -> false
                         end
                     end, Content),
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
    };
%% DSL shorthand: element name IS the action; attributes ARE adjectives.
%% on:* (starts with "on") → event trigger → preposition
%% anything else           → action        → verb
xml_to_predicate(#xmlElement{name = Name, attributes = Attrs})
  when Name =/= letter, Name =/= predicate ->
    NameBin = atom_to_binary(Name, utf8),
    {Kind, ActionBin} = classify_dsl_action(NameBin),
    {Id, Subj, Adjs} = dsl_attrs_to_adjectives(Attrs),
    #predicate{
        id         = Id,
        subject    = Subj,
        action     = {Kind, ActionBin},
        adjectives = Adjs,
        abstract   = undefined
    };
xml_to_predicate(_) ->
    undefined.

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
%% DSL helpers
%% ---------------------------------------------------------------------------

%% Split an event predicate's adjectives into {Matches, Bindings}.
%%
%%   Matches  — attributes without "_" prefix: #{field => expected_value}
%%              The event only fires when these field values match exactly.
%%
%%   Bindings — attributes with "_" prefix: #{field => variable_name}
%%              The event fires for any value; the received value is captured
%%              as `variable_name` for downstream use (template, webhook, etc.)
%%              The leading "_" is stripped from the field name in the result.
%%
%% Example:
%%   <on:complete status_code="200" _body="response_body"/>
%%   → Matches  = #{<<"status_code">> => <<"200">>}
%%   → Bindings = #{<<"body">> => <<"response_body">>}
%%
-spec event_adjectives(#predicate{}) ->
    {Matches :: #{binary() => binary()},
     Bindings :: #{binary() => binary()}}.
event_adjectives(#predicate{adjectives = Adjs}) ->
    maps:fold(fun
        (<<"_", Field/binary>>, VarName, {M, B}) ->
            {M, maps:put(Field, VarName, B)};
        (Field, Value, {M, B}) ->
            {maps:put(Field, Value, M), B}
    end, {#{}, #{}}, Adjs).

%% Two cases only:
%%   starts with "on" → event trigger → preposition
%%   anything else    → action        → verb
classify_dsl_action(<<"on", _/binary>> = A) -> {preposition, A};
classify_dsl_action(A)                      -> {verb,        A}.

%% Partition DSL element attributes into id, subject, and the adjectives map.
%% The special attributes `id` and `subject` map to the predicate record fields;
%% all others become adjective key→value pairs.
dsl_attrs_to_adjectives(Attrs) ->
    lists:foldl(fun(#xmlAttribute{name = AN, value = AV}, {Id, Subj, Adjs}) ->
        ValBin = list_to_binary(AV),
        case AN of
            id      -> {ValBin, Subj, Adjs};
            subject -> {Id, ValBin, Adjs};
            _       -> {Id, Subj, maps:put(atom_to_binary(AN, utf8), ValBin, Adjs)}
        end
    end, {<<>>, <<>>, #{}}, Attrs).

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
