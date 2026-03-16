%%
%% clexical_letter_SUITE — Pure unit tests for letter/predicate construction.
%%
%% No application is started; these tests exercise record construction,
%% JSON serialisation round-trips, and structural invariants.
%%
-module(clexical_letter_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include("../include/clexical.hrl").

-export([all/0, suite/0]).
-export([
    empty_letter/1,
    letter_with_subject/1,
    letter_decree_type/1,
    letter_bulletin_type/1,
    letter_with_single_verb/1,
    letter_with_single_preposition/1,
    letter_multi_predicate_ordering/1,
    predicate_adjectives_preserved/1,
    json_letter_roundtrip_subject/1,
    json_letter_roundtrip_predicates/1,
    json_letter_roundtrip_type/1,
    predicate_map_roundtrip_verb/1,
    predicate_map_roundtrip_preposition/1,
    nested_abstract_sub_letter/1,
    letter_wildcard_constants/1
]).

suite() -> [{timetrap, {seconds, 10}}].

all() ->
    [
        empty_letter,
        letter_with_subject,
        letter_decree_type,
        letter_bulletin_type,
        letter_with_single_verb,
        letter_with_single_preposition,
        letter_multi_predicate_ordering,
        predicate_adjectives_preserved,
        json_letter_roundtrip_subject,
        json_letter_roundtrip_predicates,
        json_letter_roundtrip_type,
        predicate_map_roundtrip_verb,
        predicate_map_roundtrip_preposition,
        nested_abstract_sub_letter,
        letter_wildcard_constants
    ].

%% ---------------------------------------------------------------------------
%% Test cases
%% ---------------------------------------------------------------------------

empty_letter(_Config) ->
    L = #letter{subject = <<>>, author = <<>>, recipient = <<>>,
                predicates = [], type = decree, via = undefined,
                envelop = undefined, original = undefined},
    ?assertEqual(<<>>, L#letter.subject),
    ?assertEqual([], L#letter.predicates).

letter_with_subject(_Config) ->
    L = #letter{subject = <<"order-42">>, type = decree, predicates = []},
    ?assertEqual(<<"order-42">>, L#letter.subject).

letter_decree_type(_Config) ->
    L = #letter{subject = <<"x">>, type = decree, predicates = []},
    ?assertEqual(decree, L#letter.type).

letter_bulletin_type(_Config) ->
    L = #letter{subject = <<"x">>, type = bulletin, predicates = []},
    ?assertEqual(bulletin, L#letter.type).

letter_with_single_verb(_Config) ->
    P = #predicate{id = <<"p1">>, action = {verb, <<"notify">>},
                   adjectives = #{<<"channel">> => <<"email">>}},
    L = #letter{subject = <<"test">>, type = decree, predicates = [P]},
    ?assertEqual(1, length(L#letter.predicates)),
    [{verb, <<"notify">>}] = [P2#predicate.action || P2 <- L#letter.predicates].

letter_with_single_preposition(_Config) ->
    P = #predicate{id = <<"s1">>, action = {preposition, <<"store">>},
                   adjectives = #{<<"bucket">> => <<"orders">>}},
    L = #letter{subject = <<"test">>, type = bulletin, predicates = [P]},
    [{preposition, <<"store">>}] = [P2#predicate.action || P2 <- L#letter.predicates].

letter_multi_predicate_ordering(_Config) ->
    P1 = #predicate{action = {preposition, <<"save">>}, adjectives = #{}},
    P2 = #predicate{action = {verb, <<"notify">>},      adjectives = #{}},
    P3 = #predicate{action = {verb, <<"webhook">>},     adjectives = #{}},
    L  = #letter{subject = <<"test">>, type = decree, predicates = [P1, P2, P3]},
    Actions = [A || #predicate{action = A} <- L#letter.predicates],
    ?assertEqual([{preposition,<<"save">>},{verb,<<"notify">>},{verb,<<"webhook">>}], Actions).

predicate_adjectives_preserved(_Config) ->
    Adjs = #{<<"field">> => <<"status">>, <<"value">> => <<"done">>},
    P = #predicate{action = {verb, <<"do:fieldUpdate">>}, adjectives = Adjs},
    ?assertEqual(<<"status">>, maps:get(<<"field">>, P#predicate.adjectives)),
    ?assertEqual(<<"done">>,   maps:get(<<"value">>, P#predicate.adjectives)).

json_letter_roundtrip_subject(_Config) ->
    Map = #{<<"subject">> => <<"my-entity-99">>,
            <<"author">>  => <<"alice">>,
            <<"type">>    => <<"decree">>,
            <<"predicates">> => []},
    L = http_herald:json_to_letter(Map),
    Back = http_herald:letter_to_json(L),
    ?assertEqual(<<"my-entity-99">>, maps:get(<<"subject">>, Back)).

json_letter_roundtrip_predicates(_Config) ->
    PMap = #{<<"action_type">> => <<"verb">>,
             <<"action">>      => <<"do:notify">>,
             <<"adjectives">>  => #{<<"channel">> => <<"slack">>}},
    Map  = #{<<"subject">>    => <<"item-1">>,
             <<"author">>     => <<"bot">>,
             <<"type">>       => <<"decree">>,
             <<"predicates">> => [PMap]},
    L    = http_herald:json_to_letter(Map),
    ?assertEqual(1, length(L#letter.predicates)),
    [P]  = L#letter.predicates,
    ?assertEqual({verb, <<"do:notify">>}, P#predicate.action),
    ?assertEqual(#{<<"channel">> => <<"slack">>}, P#predicate.adjectives).

json_letter_roundtrip_type(_Config) ->
    DecreeMap = #{<<"subject">> => <<"x">>, <<"author">> => <<"y">>,
                  <<"type">>    => <<"decree">>, <<"predicates">> => []},
    BulletinMap = DecreeMap#{<<"type">> => <<"bulletin">>},
    LD = http_herald:json_to_letter(DecreeMap),
    LB = http_herald:json_to_letter(BulletinMap),
    ?assertEqual(decree,   LD#letter.type),
    ?assertEqual(bulletin, LB#letter.type).

predicate_map_roundtrip_verb(_Config) ->
    P    = #predicate{id = <<"v1">>, subject = <<"obj-1">>,
                      action = {verb, <<"do:fieldUpdate">>},
                      adjectives = #{<<"field">> => <<"priority">>,
                                     <<"value">> => <<"high">>}},
    Map  = http_herald:predicate_to_map(P),
    P2   = http_herald:map_to_predicate(Map),
    ?assertEqual({verb, <<"do:fieldUpdate">>}, P2#predicate.action).

predicate_map_roundtrip_preposition(_Config) ->
    P  = #predicate{id = <<"s1">>, subject = <<"obj-1">>,
                    action = {preposition, <<"if:fieldValue">>},
                    adjectives = #{<<"field">> => <<"status">>,
                                   <<"op">>    => <<"eq">>,
                                   <<"value">> => <<"open">>}},
    Map = http_herald:predicate_to_map(P),
    P2  = http_herald:map_to_predicate(Map),
    ?assertEqual({preposition, <<"if:fieldValue">>}, P2#predicate.action).

nested_abstract_sub_letter(_Config) ->
    %% The abstract field carries a JSON-encoded sub-letter
    SubLetter = #{<<"subject">>    => <<"sub-entity">>,
                  <<"author">>     => <<"system">>,
                  <<"type">>       => <<"decree">>,
                  <<"predicates">> => [
                      #{<<"action_type">> => <<"verb">>,
                        <<"action">>      => <<"do:notify">>,
                        <<"adjectives">>  => #{}}
                  ]},
    SubBin = jsone:encode(SubLetter),
    P  = #predicate{action   = {verb, <<"dispatch">>},
                    abstract = SubBin,
                    adjectives = #{}},
    %% excerpts/1 should unpack the sub-letter predicates
    [SubP | _] = http_herald:excerpts(P),
    ?assertEqual({verb, <<"do:notify">>}, SubP#predicate.action).

letter_wildcard_constants(_Config) ->
    ?assertEqual(<<"*ID*">>,        ?ANY_ID),
    ?assertEqual(<<"*SUBJECT*">>,   ?ANY_SUBJECT),
    ?assertEqual(<<"*RECIPIENT*">>, ?ANY_RECIPIENT),
    ?assertEqual(<<"*AUTHOR*">>,    ?ANY_AUTHOR).
