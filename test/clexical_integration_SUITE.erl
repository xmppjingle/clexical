%%
%% clexical_integration_SUITE — End-to-end HTTP integration tests.
%%
%% 66 test cases covering:
%%   Health, Auth, Recite, Attend, Letters, Automations, Webhooks,
%%   complex multi-predicate letter scripts, and XML format.
%%
%% The full clexical application is started on port 18089 in init_per_suite
%% and stopped in end_per_suite.  All HTTP calls use httpc (OTP built-in).
%%
-module(clexical_integration_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include("../include/clexical.hrl").

-define(PORT,    18089).
-define(API_KEY, "test-api-key-clexical").
-define(KEY,     <<"test-api-key-clexical">>).

-export([all/0, groups/0, suite/0,
         init_per_suite/1, end_per_suite/1,
         init_per_group/2, end_per_group/2]).

%% Health
-export([health_check/1, health_no_auth_needed/1]).
%% Auth
-export([auth_x_api_key/1, auth_bearer/1, auth_query_param/1,
         auth_invalid_key/1, auth_missing_key/1]).
%% Recite
-export([recite_simple_decree/1, recite_multi_predicate/1,
         recite_with_adjectives/1, recite_bulletin/1,
         recite_returns_202/1, recite_bad_json/1,
         recite_empty_predicates/1, recite_on_trigger_predicate/1]).
%% Attend
-export([attend_returns_202/1, attend_bad_json/1,
         attend_decree/1, attend_bulletin/1, attend_complex_predicates/1]).
%% Letters
-export([letters_get/1, letters_post_decree/1, letters_post_bulletin/1,
         letters_get_with_qs_filter/1, letters_bad_json/1]).
%% Automations
-export([automations_list_empty/1, automations_register_interval/1,
         automations_register_once/1, automations_list_after_register/1,
         automations_register_returns_id/1, automations_delete/1,
         automations_delete_not_found/1, automations_bad_schedule/1,
         automations_complex_letter/1, automations_multiple/1]).
%% Webhooks
-export([webhooks_register/1, webhooks_list/1, webhooks_get_by_id/1,
         webhooks_delete/1, webhooks_delete_not_found/1,
         webhooks_inbound_get/1, webhooks_inbound_post_json/1,
         webhooks_inbound_qs_params/1, webhooks_inbound_unknown/1,
         webhooks_inbound_valid_hmac/1, webhooks_inbound_invalid_hmac/1,
         webhooks_register_and_trigger/1]).
%% XML format
-export([xml_recite_simple/1, xml_recite_multi_predicate/1,
         xml_attend_decree/1, xml_roundtrip_json_to_xml/1,
         xml_roundtrip_xml_to_json/1, xml_content_type_header/1,
         xml_accept_header/1, xml_nested_abstract/1]).
%% Complex scripts
-export([script_on_if_do_full/1, script_nested_abstract/1,
         script_three_actions/1, script_mixed_preposition_verb/1,
         script_full_rule_trigger/1, script_automation_fires_letter/1,
         script_do_webhook_outbound_get/1, script_do_webhook_outbound_post/1,
         script_concurrent_recite/1, script_unicode_adjectives/1,
         script_deep_nested_abstract/1]).

suite() -> [{timetrap, {seconds, 30}}].

all() ->
    [{group, health},
     {group, auth},
     {group, recite},
     {group, attend},
     {group, letters},
     {group, automations},
     {group, webhooks},
     {group, xml},
     {group, scripts}].

groups() ->
    [
        {health,      [], [health_check, health_no_auth_needed]},
        {auth,        [], [auth_x_api_key, auth_bearer, auth_query_param,
                           auth_invalid_key, auth_missing_key]},
        {recite,      [], [recite_simple_decree, recite_multi_predicate,
                           recite_with_adjectives, recite_bulletin,
                           recite_returns_202, recite_bad_json,
                           recite_empty_predicates, recite_on_trigger_predicate]},
        {attend,      [], [attend_returns_202, attend_bad_json,
                           attend_decree, attend_bulletin, attend_complex_predicates]},
        {letters,     [], [letters_get, letters_post_decree, letters_post_bulletin,
                           letters_get_with_qs_filter, letters_bad_json]},
        {automations, [], [automations_list_empty, automations_register_interval,
                           automations_register_once, automations_list_after_register,
                           automations_register_returns_id, automations_delete,
                           automations_delete_not_found, automations_bad_schedule,
                           automations_complex_letter, automations_multiple]},
        {webhooks,    [], [webhooks_register, webhooks_list, webhooks_get_by_id,
                           webhooks_delete, webhooks_delete_not_found,
                           webhooks_inbound_get, webhooks_inbound_post_json,
                           webhooks_inbound_qs_params, webhooks_inbound_unknown,
                           webhooks_inbound_valid_hmac, webhooks_inbound_invalid_hmac,
                           webhooks_register_and_trigger]},
        {xml,         [], [xml_recite_simple, xml_recite_multi_predicate,
                           xml_attend_decree, xml_roundtrip_json_to_xml,
                           xml_roundtrip_xml_to_json, xml_content_type_header,
                           xml_accept_header, xml_nested_abstract]},
        {scripts,     [], [script_on_if_do_full, script_nested_abstract,
                           script_three_actions, script_mixed_preposition_verb,
                           script_full_rule_trigger, script_automation_fires_letter,
                           script_do_webhook_outbound_get, script_do_webhook_outbound_post,
                           script_concurrent_recite, script_unicode_adjectives,
                           script_deep_nested_abstract]}
    ].

init_per_suite(Config) ->
    ok = ct_helper:start_app(?PORT, ?API_KEY),
    Config.

end_per_suite(_Config) ->
    ct_helper:stop_app().

init_per_group(_Group, Config) -> Config.
end_per_group(_Group, _Config) -> ok.

%% ---------------------------------------------------------------------------
%% HEALTH
%% ---------------------------------------------------------------------------

health_check(_Config) ->
    R = ct_helper:http_no_auth_get(url("/health")),
    ?assertEqual(200, ct_helper:status(R)),
    M = ct_helper:decode_body(R),
    ?assertEqual(<<"ok">>, maps:get(<<"status">>, M)).

health_no_auth_needed(_Config) ->
    %% /health must return 200 even with a wrong key
    R = ct_helper:http_get(url("/health"), <<"wrong-key">>),
    ?assertEqual(200, ct_helper:status(R)).

%% ---------------------------------------------------------------------------
%% AUTH
%% ---------------------------------------------------------------------------

auth_x_api_key(_Config) ->
    R = ct_helper:http_post(api("/recite"), ?KEY, simple_letter()),
    ?assertEqual(202, ct_helper:status(R)).

auth_bearer(_Config) ->
    R = ct_helper:http_bearer(api("/recite"), ?KEY, simple_letter()),
    ?assertEqual(202, ct_helper:status(R)).

auth_query_param(_Config) ->
    R = ct_helper:http_qs_key(api("/recite"), ?KEY, simple_letter()),
    ?assertEqual(202, ct_helper:status(R)).

auth_invalid_key(_Config) ->
    R = ct_helper:http_post(api("/recite"), <<"bad-key">>, simple_letter()),
    ?assertEqual(401, ct_helper:status(R)),
    M = ct_helper:decode_body(R),
    ?assertEqual(<<"unauthorized">>, maps:get(<<"error">>, M)).

auth_missing_key(_Config) ->
    R = ct_helper:http_no_auth_post(api("/recite"), simple_letter()),
    ?assertEqual(401, ct_helper:status(R)).

%% ---------------------------------------------------------------------------
%% RECITE
%% ---------------------------------------------------------------------------

recite_simple_decree(_Config) ->
    Body = letter(uid(<<"recite-simple">>), [verb(<<"do:notify">>, #{<<"to">> => <<"alice">>})]),
    R = ct_helper:http_post(api("/recite"), ?KEY, Body),
    ?assertEqual(202, ct_helper:status(R)).

recite_multi_predicate(_Config) ->
    Preds = [
        verb(<<"do:fieldUpdate">>, #{<<"field">> => <<"status">>, <<"value">> => <<"done">>}),
        verb(<<"do:notify">>,      #{<<"to">>    => <<"alice">>}),
        verb(<<"do:webhook">>,     #{<<"url">>   => <<"http://localhost:18089/health">>,
                                     <<"method">> => <<"GET">>})
    ],
    R = ct_helper:http_post(api("/recite"), ?KEY, letter(uid(<<"multi">>), Preds)),
    ?assertEqual(202, ct_helper:status(R)).

recite_with_adjectives(_Config) ->
    Adjs = #{
        <<"field">>    => <<"priority">>,
        <<"old_value">>=> <<"low">>,
        <<"new_value">>=> <<"critical">>,
        <<"reason">>   => <<"SLA breach">>
    },
    R = ct_helper:http_post(api("/recite"), ?KEY,
            letter(uid(<<"adjs">>), [verb(<<"do:fieldUpdate">>, Adjs)])),
    ?assertEqual(202, ct_helper:status(R)).

recite_bulletin(_Config) ->
    B = ct_helper:make_bulletin(uid(<<"bulletin-recite">>), <<"system">>,
            [verb(<<"do:notify">>, #{})]),
    R = ct_helper:http_post(api("/recite"), ?KEY, B),
    ?assertEqual(202, ct_helper:status(R)).

recite_returns_202(_Config) ->
    R = ct_helper:http_post(api("/recite"), ?KEY, simple_letter()),
    M = ct_helper:decode_body(R),
    ?assertEqual(<<"reciting">>, maps:get(<<"status">>, M)).

recite_bad_json(_Config) ->
    R = ct_helper:http_post(api("/recite"), ?KEY, <<"not json at all {">>),
    ?assertEqual(400, ct_helper:status(R)).

recite_empty_predicates(_Config) ->
    R = ct_helper:http_post(api("/recite"), ?KEY,
            ct_helper:make_letter(uid(<<"empty-preds">>), [])),
    ?assertEqual(202, ct_helper:status(R)).

recite_on_trigger_predicate(_Config) ->
    %% on:* trigger predicates should be dispatched without error
    Preds = [verb(<<"on:statusChange">>, #{<<"field">> => <<"status">>,
                                           <<"from">>  => <<"open">>,
                                           <<"to">>    => <<"closed">>})],
    R = ct_helper:http_post(api("/recite"), ?KEY, letter(uid(<<"trigger">>), Preds)),
    ?assertEqual(202, ct_helper:status(R)).

%% ---------------------------------------------------------------------------
%% ATTEND
%% ---------------------------------------------------------------------------

attend_returns_202(_Config) ->
    R = ct_helper:http_post(api("/attend"), ?KEY, simple_letter()),
    M = ct_helper:decode_body(R),
    ?assertEqual(<<"attending">>, maps:get(<<"status">>, M)).

attend_bad_json(_Config) ->
    R = ct_helper:http_post(api("/attend"), ?KEY, <<"{bad">>),
    ?assertEqual(400, ct_helper:status(R)).

attend_decree(_Config) ->
    Preds = [prep(<<"store:field">>, #{<<"field">> => <<"state">>, <<"value">> => <<"active">>})],
    R = ct_helper:http_post(api("/attend"), ?KEY, letter(uid(<<"attend-decree">>), Preds)),
    ?assertEqual(202, ct_helper:status(R)).

attend_bulletin(_Config) ->
    B = ct_helper:make_bulletin(uid(<<"attend-bulletin">>), <<"system">>,
            [prep(<<"store:meta">>, #{<<"tag">> => <<"urgent">>})]),
    R = ct_helper:http_post(api("/attend"), ?KEY, B),
    ?assertEqual(202, ct_helper:status(R)).

attend_complex_predicates(_Config) ->
    Preds = [
        prep(<<"if:fieldValue">>, #{<<"field">> => <<"status">>,
                                    <<"op">>    => <<"eq">>,
                                    <<"value">> => <<"open">>}),
        prep(<<"if:actor">>,      #{<<"actor">> => <<"admin">>}),
        verb(<<"do:fieldUpdate">>,#{<<"field">> => <<"priority">>,
                                    <<"value">> => <<"high">>}),
        verb(<<"do:notify">>,     #{<<"channel">> => <<"ops">>})
    ],
    R = ct_helper:http_post(api("/attend"), ?KEY, letter(uid(<<"attend-cx">>), Preds)),
    ?assertEqual(202, ct_helper:status(R)).

%% ---------------------------------------------------------------------------
%% LETTERS
%% ---------------------------------------------------------------------------

letters_get(_Config) ->
    R = ct_helper:http_get(api("/letters"), ?KEY),
    ?assertEqual(200, ct_helper:status(R)),
    M = ct_helper:decode_body(R),
    ?assert(is_list(maps:get(<<"predicates">>, M))).

letters_post_decree(_Config) ->
    Preds = [verb(<<"do:fieldUpdate">>, #{<<"field">> => <<"state">>})],
    R = ct_helper:http_post(api("/letters"), ?KEY, letter(uid(<<"letters-d">>), Preds)),
    ?assertEqual(202, ct_helper:status(R)).

letters_post_bulletin(_Config) ->
    B = ct_helper:make_bulletin(uid(<<"letters-b">>), <<"system">>, []),
    R = ct_helper:http_post(api("/letters"), ?KEY, B),
    ?assertEqual(202, ct_helper:status(R)).

letters_get_with_qs_filter(_Config) ->
    %% Filter by a subject that won't exist — still returns 200 with empty list
    R = ct_helper:http_get(api("/letters?subject=nonexistent-subject-xyz"), ?KEY),
    ?assertEqual(200, ct_helper:status(R)),
    M = ct_helper:decode_body(R),
    ?assertEqual([], maps:get(<<"predicates">>, M)).

letters_bad_json(_Config) ->
    R = ct_helper:http_post(api("/letters"), ?KEY, <<"{invalid">>),
    ?assertEqual(400, ct_helper:status(R)).

%% ---------------------------------------------------------------------------
%% AUTOMATIONS
%% ---------------------------------------------------------------------------

automations_list_empty(_Config) ->
    %% List may have pre-registered items from other tests; just verify structure
    R = ct_helper:http_get(api("/automations"), ?KEY),
    ?assertEqual(200, ct_helper:status(R)),
    M = ct_helper:decode_body(R),
    ?assert(is_list(maps:get(<<"automations">>, M))).

automations_register_interval(_Config) ->
    Auto = #{
        <<"id">>       => uid(<<"auto-interval">>),
        <<"name">>     => <<"Hourly sync">>,
        <<"schedule">> => #{<<"type">> => <<"interval">>, <<"ms">> => 3600000},
        <<"letter">>   => jsone:decode(simple_letter(), [{object_format, map}])
    },
    R = ct_helper:http_post(api("/automations"), ?KEY, jsone:encode(Auto)),
    ?assertEqual(201, ct_helper:status(R)),
    M = ct_helper:decode_body(R),
    ?assertEqual(<<"registered">>, maps:get(<<"status">>, M)).

automations_register_once(_Config) ->
    FutureMs = erlang:system_time(millisecond) + 3_600_000,
    Auto = #{
        <<"id">>       => uid(<<"auto-once">>),
        <<"name">>     => <<"One-shot">>,
        <<"schedule">> => #{<<"type">> => <<"once">>, <<"at">> => FutureMs},
        <<"letter">>   => jsone:decode(simple_letter(), [{object_format, map}])
    },
    R = ct_helper:http_post(api("/automations"), ?KEY, jsone:encode(Auto)),
    ?assertEqual(201, ct_helper:status(R)).

automations_list_after_register(_Config) ->
    Id = uid(<<"auto-list-check">>),
    Auto = #{
        <<"id">>       => Id,
        <<"name">>     => <<"List check">>,
        <<"schedule">> => #{<<"type">> => <<"interval">>, <<"ms">> => 60000},
        <<"letter">>   => jsone:decode(simple_letter(), [{object_format, map}])
    },
    ct_helper:http_post(api("/automations"), ?KEY, jsone:encode(Auto)),
    R    = ct_helper:http_get(api("/automations"), ?KEY),
    M    = ct_helper:decode_body(R),
    Autos = maps:get(<<"automations">>, M),
    Ids   = [maps:get(<<"id">>, A, undefined) || A <- Autos],
    ?assert(lists:member(Id, Ids)).

automations_register_returns_id(_Config) ->
    Id = uid(<<"auto-id-check">>),
    Auto = #{
        <<"id">>       => Id,
        <<"name">>     => <<"ID check">>,
        <<"schedule">> => #{<<"type">> => <<"interval">>, <<"ms">> => 5000},
        <<"letter">>   => jsone:decode(simple_letter(), [{object_format, map}])
    },
    R = ct_helper:http_post(api("/automations"), ?KEY, jsone:encode(Auto)),
    M = ct_helper:decode_body(R),
    ?assertEqual(Id, maps:get(<<"id">>, M)).

automations_delete(_Config) ->
    Id = uid(<<"auto-del">>),
    Auto = #{
        <<"id">>       => Id,
        <<"name">>     => <<"Delete me">>,
        <<"schedule">> => #{<<"type">> => <<"interval">>, <<"ms">> => 99999},
        <<"letter">>   => jsone:decode(simple_letter(), [{object_format, map}])
    },
    ct_helper:http_post(api("/automations"), ?KEY, jsone:encode(Auto)),
    DelUrl = api("/automations/" ++ binary_to_list(Id)),
    R = ct_helper:http_delete(DelUrl, ?KEY),
    ?assertEqual(200, ct_helper:status(R)).

automations_delete_not_found(_Config) ->
    R = ct_helper:http_delete(api("/automations/no-such-automation-xyz"), ?KEY),
    ?assertEqual(404, ct_helper:status(R)).

automations_bad_schedule(_Config) ->
    Auto = #{
        <<"id">>       => uid(<<"auto-bad">>),
        <<"name">>     => <<"Bad schedule">>,
        <<"schedule">> => #{<<"type">> => <<"unknown">>},
        <<"letter">>   => jsone:decode(simple_letter(), [{object_format, map}])
    },
    R = ct_helper:http_post(api("/automations"), ?KEY, jsone:encode(Auto)),
    ?assertEqual(422, ct_helper:status(R)).

automations_complex_letter(_Config) ->
    Preds = [
        verb(<<"on:schedule">>,    #{<<"cron">> => <<"0 * * * *">>}),
        verb(<<"do:webhook">>,     #{<<"url">>  => <<"http://localhost:18089/health">>,
                                     <<"method">> => <<"GET">>}),
        verb(<<"do:notify">>,      #{<<"channel">> => <<"ops">>,
                                     <<"message">>  => <<"Hourly check completed">>})
    ],
    Auto = #{
        <<"id">>       => uid(<<"auto-complex">>),
        <<"name">>     => <<"Complex hourly">>,
        <<"schedule">> => #{<<"type">> => <<"interval">>, <<"ms">> => 3600000},
        <<"letter">>   => jsone:decode(letter(uid(<<"auto-l">>), Preds),
                                       [{object_format, map}])
    },
    R = ct_helper:http_post(api("/automations"), ?KEY, jsone:encode(Auto)),
    ?assertEqual(201, ct_helper:status(R)).

automations_multiple(_Config) ->
    %% Register 5 automations and verify all appear in the list
    Ids = [uid(<<"multi-auto">>) || _ <- lists:seq(1, 5)],
    lists:foreach(fun(Id) ->
        Auto = #{
            <<"id">>       => Id,
            <<"name">>     => <<"Auto ", Id/binary>>,
            <<"schedule">> => #{<<"type">> => <<"interval">>, <<"ms">> => 10000},
            <<"letter">>   => jsone:decode(simple_letter(), [{object_format, map}])
        },
        ct_helper:http_post(api("/automations"), ?KEY, jsone:encode(Auto))
    end, Ids),
    R    = ct_helper:http_get(api("/automations"), ?KEY),
    M    = ct_helper:decode_body(R),
    Listed = maps:get(<<"automations">>, M),
    ListedIds = [maps:get(<<"id">>, A) || A <- Listed],
    lists:foreach(fun(Id) ->
        ?assert(lists:member(Id, ListedIds))
    end, Ids).

%% ---------------------------------------------------------------------------
%% WEBHOOKS
%% ---------------------------------------------------------------------------

webhooks_register(_Config) ->
    Id = uid(<<"wh-register">>),
    Reg = webhook_reg(Id, undefined),
    R = ct_helper:http_post(api("/webhooks"), ?KEY, jsone:encode(Reg)),
    ?assertEqual(201, ct_helper:status(R)),
    M = ct_helper:decode_body(R),
    ?assertEqual(Id, maps:get(<<"id">>, M)).

webhooks_list(_Config) ->
    R = ct_helper:http_get(api("/webhooks"), ?KEY),
    ?assertEqual(200, ct_helper:status(R)),
    M = ct_helper:decode_body(R),
    ?assert(is_list(maps:get(<<"webhooks">>, M))).

webhooks_get_by_id(_Config) ->
    Id = uid(<<"wh-get">>),
    ct_helper:http_post(api("/webhooks"), ?KEY, jsone:encode(webhook_reg(Id, undefined))),
    R = ct_helper:http_get(api("/webhooks/" ++ binary_to_list(Id)), ?KEY),
    ?assertEqual(200, ct_helper:status(R)),
    M = ct_helper:decode_body(R),
    ?assertEqual(Id, maps:get(<<"id">>, M)).

webhooks_delete(_Config) ->
    Id = uid(<<"wh-del">>),
    ct_helper:http_post(api("/webhooks"), ?KEY, jsone:encode(webhook_reg(Id, undefined))),
    R = ct_helper:http_delete(api("/webhooks/" ++ binary_to_list(Id)), ?KEY),
    ?assertEqual(200, ct_helper:status(R)).

webhooks_delete_not_found(_Config) ->
    R = ct_helper:http_delete(api("/webhooks/no-such-wh-xyz"), ?KEY),
    ?assertEqual(404, ct_helper:status(R)).

webhooks_inbound_get(_Config) ->
    Id = uid(<<"wh-in-get">>),
    ct_helper:http_post(api("/webhooks"), ?KEY, jsone:encode(webhook_reg(Id, undefined))),
    R = ct_helper:http_no_auth_get(api("/webhooks/in/" ++ binary_to_list(Id))),
    ?assertEqual(200, ct_helper:status(R)),
    M = ct_helper:decode_body(R),
    ?assertEqual(<<"dispatched">>, maps:get(<<"status">>, M)).

webhooks_inbound_post_json(_Config) ->
    Id = uid(<<"wh-in-post">>),
    ct_helper:http_post(api("/webhooks"), ?KEY, jsone:encode(webhook_reg(Id, undefined))),
    Payload = jsone:encode(#{<<"event">> => <<"order.created">>,
                              <<"order_id">> => <<"ORD-999">>}),
    R = ct_helper:http_no_auth_post(api("/webhooks/in/" ++ binary_to_list(Id)), Payload),
    ?assertEqual(200, ct_helper:status(R)),
    M = ct_helper:decode_body(R),
    ?assertEqual(<<"dispatched">>, maps:get(<<"status">>, M)).

webhooks_inbound_qs_params(_Config) ->
    Id = uid(<<"wh-in-qs">>),
    ct_helper:http_post(api("/webhooks"), ?KEY, jsone:encode(webhook_reg(Id, undefined))),
    Url = api("/webhooks/in/" ++ binary_to_list(Id) ++ "?source=crm&action=sync"),
    R = ct_helper:http_no_auth_get(Url),
    ?assertEqual(200, ct_helper:status(R)).

webhooks_inbound_unknown(_Config) ->
    R = ct_helper:http_no_auth_post(api("/webhooks/in/no-such-hook-xyz"),
                                    jsone:encode(#{})),
    ?assertEqual(404, ct_helper:status(R)).

webhooks_inbound_valid_hmac(_Config) ->
    Id     = uid(<<"wh-hmac-ok">>),
    Secret = <<"my-hmac-secret-key">>,
    Reg    = (webhook_reg(Id, undefined))#{<<"secret">> => Secret},
    ct_helper:http_post(api("/webhooks"), ?KEY, jsone:encode(Reg)),
    Body   = jsone:encode(#{<<"event">> => <<"test">>}),
    R      = ct_helper:http_hmac_post(
                 api("/webhooks/in/" ++ binary_to_list(Id)),
                 Secret, Body, []),
    ?assertEqual(200, ct_helper:status(R)).

webhooks_inbound_invalid_hmac(_Config) ->
    Id     = uid(<<"wh-hmac-bad">>),
    Secret = <<"real-secret">>,
    Reg    = (webhook_reg(Id, undefined))#{<<"secret">> => Secret},
    ct_helper:http_post(api("/webhooks"), ?KEY, jsone:encode(Reg)),
    Body   = jsone:encode(#{<<"event">> => <<"test">>}),
    R      = ct_helper:http_hmac_post(
                 api("/webhooks/in/" ++ binary_to_list(Id)),
                 <<"wrong-secret">>, Body, []),
    ?assertEqual(401, ct_helper:status(R)).

webhooks_register_and_trigger(_Config) ->
    Id = uid(<<"wh-full">>),
    %% Template with Mustache placeholder for the incoming field
    LetterTemplate = #{
        <<"subject">>    => <<"external-event">>,
        <<"author">>     => <<"webhook">>,
        <<"type">>       => <<"decree">>,
        <<"predicates">> => [
            #{<<"action_type">> => <<"verb">>,
              <<"action">>      => <<"do:notify">>,
              <<"adjectives">>  => #{<<"event">> => <<"{{event}}">>}}
        ]
    },
    Reg = #{<<"id">> => Id, <<"letter">> => LetterTemplate},
    ct_helper:http_post(api("/webhooks"), ?KEY, jsone:encode(Reg)),
    Payload = jsone:encode(#{<<"event">> => <<"order.shipped">>}),
    R = ct_helper:http_no_auth_post(api("/webhooks/in/" ++ binary_to_list(Id)), Payload),
    ?assertEqual(200, ct_helper:status(R)).

%% ---------------------------------------------------------------------------
%% COMPLEX SCRIPTS (multi-predicate letters)
%% ---------------------------------------------------------------------------

script_on_if_do_full(_Config) ->
    %% Full trigger → condition → action pipeline
    Preds = [
        verb(<<"on:statusChange">>, #{<<"field">>  => <<"status">>,
                                      <<"from">>   => <<"pending">>,
                                      <<"to">>     => <<"approved">>}),
        prep(<<"if:status">>,       #{<<"op">>     => <<"eq">>,
                                      <<"value">>  => <<"approved">>}),
        prep(<<"if:actor">>,        #{<<"actor">>  => <<"manager">>}),
        verb(<<"do:fieldUpdate">>,  #{<<"field">>  => <<"reviewed_by">>,
                                      <<"value">>  => <<"manager">>}),
        verb(<<"do:notify">>,       #{<<"channel">>=> <<"email">>,
                                      <<"to">>     => <<"applicant">>})
    ],
    R = ct_helper:http_post(api("/recite"), ?KEY, letter(uid(<<"on-if-do">>), Preds)),
    ?assertEqual(202, ct_helper:status(R)).

script_nested_abstract(_Config) ->
    %% A predicate whose abstract field contains a sub-letter
    SubLetter = #{
        <<"subject">>    => <<"sub-order">>,
        <<"author">>     => <<"system">>,
        <<"type">>       => <<"decree">>,
        <<"predicates">> => [
            #{<<"action_type">> => <<"verb">>,
              <<"action">>      => <<"do:notify">>,
              <<"adjectives">>  => #{<<"channel">> => <<"ops">>}}
        ]
    },
    Preds = [
        #{<<"action_type">> => <<"verb">>,
          <<"action">>      => <<"dispatch">>,
          <<"adjectives">>  => #{},
          <<"abstract">>    => jsone:encode(SubLetter)}
    ],
    R = ct_helper:http_post(api("/recite"), ?KEY,
            jsone:encode(#{<<"subject">>    => uid(<<"nested-abs">>),
                           <<"author">>     => <<"test">>,
                           <<"type">>       => <<"decree">>,
                           <<"predicates">> => Preds})),
    ?assertEqual(202, ct_helper:status(R)).

script_three_actions(_Config) ->
    Preds = [
        verb(<<"do:fieldUpdate">>, #{<<"field">> => <<"state">>,
                                     <<"value">> => <<"processing">>}),
        verb(<<"do:transitionStatus">>, #{<<"to">> => <<"in_progress">>}),
        verb(<<"do:notify">>,           #{<<"channel">> => <<"slack">>,
                                         <<"message">>  => <<"Order is processing">>})
    ],
    R = ct_helper:http_post(api("/recite"), ?KEY, letter(uid(<<"three-actions">>), Preds)),
    ?assertEqual(202, ct_helper:status(R)).

script_mixed_preposition_verb(_Config) ->
    %% Interleaved store and execute predicates
    Preds = [
        prep(<<"store:context">>,   #{<<"key">> => <<"workflow">>,
                                      <<"val">> => <<"approval">>}),
        verb(<<"do:fieldUpdate">>,  #{<<"field">> => <<"phase">>,
                                      <<"value">> => <<"review">>}),
        prep(<<"if:fieldValue">>,   #{<<"field">> => <<"phase">>,
                                      <<"op">>    => <<"eq">>,
                                      <<"value">> => <<"review">>}),
        verb(<<"do:notify">>,       #{<<"to">> => <<"reviewer">>})
    ],
    R = ct_helper:http_post(api("/recite"), ?KEY, letter(uid(<<"mixed-pv">>), Preds)),
    ?assertEqual(202, ct_helper:status(R)).

script_full_rule_trigger(_Config) ->
    %% Full automation rule script: on:create → conditions → actions
    Preds = [
        verb(<<"on:create">>,           #{<<"entity">> => <<"order">>}),
        prep(<<"if:fieldValue">>,       #{<<"field">> => <<"amount">>,
                                          <<"op">>    => <<"gt">>,
                                          <<"value">> => <<"1000">>}),
        prep(<<"if:relatedItem">>,      #{<<"relation">> => <<"customer">>,
                                          <<"field">>    => <<"tier">>,
                                          <<"op">>       => <<"eq">>,
                                          <<"value">>    => <<"premium">>}),
        verb(<<"do:transitionStatus">>, #{<<"to">> => <<"priority_review">>}),
        verb(<<"do:notify">>,           #{<<"channel">> => <<"vip_team">>,
                                         <<"message">>  => <<"VIP high-value order">>}),
        verb(<<"do:webhook">>,          #{<<"url">>    => <<"http://localhost:18089/health">>,
                                          <<"method">> => <<"GET">>})
    ],
    R = ct_helper:http_post(api("/recite"), ?KEY, letter(uid(<<"full-rule">>), Preds)),
    ?assertEqual(202, ct_helper:status(R)).

script_automation_fires_letter(_Config) ->
    %% Register an automation with a complex letter, then trigger it manually
    Preds = [
        verb(<<"do:fieldUpdate">>, #{<<"field">> => <<"last_checked">>,
                                     <<"value">> => <<"now">>}),
        verb(<<"do:notify">>,      #{<<"channel">> => <<"monitoring">>})
    ],
    Id = uid(<<"auto-fire">>),
    Auto = #{
        <<"id">>       => Id,
        <<"name">>     => <<"Health Check Automation">>,
        <<"schedule">> => #{<<"type">> => <<"interval">>, <<"ms">> => 3_600_000},
        <<"letter">>   => jsone:decode(letter(uid(<<"auto-fire-l">>), Preds),
                                       [{object_format, map}])
    },
    R1 = ct_helper:http_post(api("/automations"), ?KEY, jsone:encode(Auto)),
    ?assertEqual(201, ct_helper:status(R1)),
    %% Trigger it now via triggerAutomation verb
    TriggerLetter = ct_helper:make_letter(
        uid(<<"trigger-req">>),
        [ct_helper:make_verb(<<"triggerAutomation">>,
                             #{<<"id">> => Id})]),
    R2 = ct_helper:http_post(api("/recite"), ?KEY, TriggerLetter),
    ?assertEqual(202, ct_helper:status(R2)).

script_do_webhook_outbound_get(_Config) ->
    %% Outbound GET webhook targeting the app's own /health endpoint
    Preds = [
        verb(<<"do:webhook">>, #{<<"url">>     => <<"http://localhost:18089/health">>,
                                  <<"method">>  => <<"GET">>,
                                  <<"timeout">> => 3000})
    ],
    R = ct_helper:http_post(api("/recite"), ?KEY, letter(uid(<<"wh-out-get">>), Preds)),
    ?assertEqual(202, ct_helper:status(R)).

script_do_webhook_outbound_post(_Config) ->
    %% Outbound POST webhook — note: /health ignores the body but returns 200
    Preds = [
        verb(<<"do:webhook">>, #{<<"url">>    => <<"http://localhost:18089/health">>,
                                  <<"method">> => <<"POST">>,
                                  <<"body">>   => <<"{\"ping\":true}">>,
                                  <<"headers">>=> #{<<"content-type">> => <<"application/json">>},
                                  <<"timeout">>=> 3000})
    ],
    R = ct_helper:http_post(api("/recite"), ?KEY, letter(uid(<<"wh-out-post">>), Preds)),
    ?assertEqual(202, ct_helper:status(R)).

script_concurrent_recite(_Config) ->
    %% Fire 10 letters concurrently and expect all to return 202
    Parent = self(),
    N = 10,
    [spawn(fun() ->
        Body = letter(uid(<<"concurrent">>),
                      [verb(<<"do:notify">>, #{<<"seq">> => integer_to_binary(I)})]),
        R = ct_helper:http_post(api("/recite"), ?KEY, Body),
        Parent ! {result, ct_helper:status(R)}
    end) || I <- lists:seq(1, N)],
    Statuses = [receive {result, S} -> S after 10000 -> timeout end
                || _ <- lists:seq(1, N)],
    ?assert(lists:all(fun(S) -> S =:= 202 end, Statuses)).

script_unicode_adjectives(_Config) ->
    Adjs = #{
        <<"title">>      => <<"Ünícödé tëxt"/utf8>>,
        <<"description">>=> <<"日本語テスト"/utf8>>,
        <<"emoji">>      => <<"🚀 launched"/utf8>>
    },
    R = ct_helper:http_post(api("/recite"), ?KEY,
            letter(uid(<<"unicode">>), [verb(<<"do:notify">>, Adjs)])),
    ?assertEqual(202, ct_helper:status(R)).

script_deep_nested_abstract(_Config) ->
    %% Three levels of abstract nesting
    Level3 = #{<<"subject">> => uid(<<"l3">>), <<"author">> => <<"s">>,
               <<"type">>    => <<"decree">>,
               <<"predicates">> => [
                   #{<<"action_type">> => <<"verb">>,
                     <<"action">>      => <<"do:notify">>,
                     <<"adjectives">>  => #{<<"level">> => <<"3">>}}
               ]},
    Level2 = #{<<"subject">> => uid(<<"l2">>), <<"author">> => <<"s">>,
               <<"type">>    => <<"decree">>,
               <<"predicates">> => [
                   #{<<"action_type">> => <<"verb">>,
                     <<"action">>      => <<"dispatch">>,
                     <<"adjectives">>  => #{},
                     <<"abstract">>    => jsone:encode(Level3)}
               ]},
    Level1Preds = [
        #{<<"action_type">> => <<"verb">>,
          <<"action">>      => <<"dispatch">>,
          <<"adjectives">>  => #{},
          <<"abstract">>    => jsone:encode(Level2)}
    ],
    R = ct_helper:http_post(api("/recite"), ?KEY,
            jsone:encode(#{<<"subject">>    => uid(<<"deep-nest">>),
                           <<"author">>     => <<"test">>,
                           <<"type">>       => <<"decree">>,
                           <<"predicates">> => Level1Preds})),
    ?assertEqual(202, ct_helper:status(R)).

%% ---------------------------------------------------------------------------
%% XML FORMAT TESTS
%% ---------------------------------------------------------------------------

xml_recite_simple(_Config) ->
    Body = <<"<letter subject=\"order-xml-1\" author=\"test\" type=\"decree\">\n"
             "  <predicate action_type=\"verb\" action=\"do:notify\">\n"
             "    <channel>slack</channel>\n"
             "    <to>#ops</to>\n"
             "  </predicate>\n"
             "</letter>">>,
    R = xml_post(api("/recite"), Body),
    ?assertEqual(202, ct_helper:status(R)).

xml_recite_multi_predicate(_Config) ->
    Body = <<"<letter subject=\"order-xml-2\" author=\"checkout\" type=\"decree\">\n"
             "  <predicate action_type=\"verb\" action=\"on:create\">\n"
             "    <entity>order</entity>\n"
             "    <source>web</source>\n"
             "  </predicate>\n"
             "  <predicate action_type=\"preposition\" action=\"if:fieldValue\">\n"
             "    <field>amount</field>\n"
             "    <op>gt</op>\n"
             "    <value>500</value>\n"
             "  </predicate>\n"
             "  <predicate action_type=\"verb\" action=\"do:transitionStatus\">\n"
             "    <to>pending_review</to>\n"
             "  </predicate>\n"
             "  <predicate action_type=\"verb\" action=\"do:notify\">\n"
             "    <channel>email</channel>\n"
             "    <to>reviewer@example.com</to>\n"
             "    <message>High-value order needs review</message>\n"
             "  </predicate>\n"
             "</letter>">>,
    R = xml_post(api("/recite"), Body),
    ?assertEqual(202, ct_helper:status(R)).

xml_attend_decree(_Config) ->
    Body = <<"<letter subject=\"ticket-xml-1\" author=\"system\" type=\"decree\">\n"
             "  <predicate action_type=\"preposition\" action=\"if:status\">\n"
             "    <op>eq</op>\n"
             "    <value>open</value>\n"
             "  </predicate>\n"
             "  <predicate action_type=\"verb\" action=\"do:fieldUpdate\">\n"
             "    <field>priority</field>\n"
             "    <value>high</value>\n"
             "  </predicate>\n"
             "</letter>">>,
    R = xml_post(api("/attend"), Body),
    ?assertEqual(202, ct_helper:status(R)).

xml_roundtrip_json_to_xml(_Config) ->
    %% Send JSON, verify the engine accepted it (202); then check xml_letter
    %% can roundtrip the same letter.
    JsonBody = ct_helper:make_letter(uid(<<"rt-json">>),
                   [ct_helper:make_verb(<<"do:notify">>,
                                        #{<<"channel">> => <<"ops">>})]),
    {ok, {{_, 202, _}, _, _}} = ct_helper:http_post(api("/recite"), ?KEY, JsonBody),
    %% Now construct the equivalent letter with xml_letter and check it parses
    XmlBody = <<"<letter subject=\"rt-xml\" author=\"system\" type=\"decree\">\n"
                "  <predicate action_type=\"verb\" action=\"do:notify\">\n"
                "    <channel>ops</channel>\n"
                "  </predicate>\n"
                "</letter>">>,
    Letter = xml_letter:from_binary(XmlBody),
    ?assertMatch(#letter{type = decree}, Letter),
    [P] = Letter#letter.predicates,
    ?assertEqual({verb, <<"do:notify">>}, P#predicate.action),
    ?assertEqual(#{<<"channel">> => <<"ops">>}, P#predicate.adjectives).

xml_roundtrip_xml_to_json(_Config) ->
    %% Parse XML letter, serialise to JSON, parse back — fields preserved
    XmlBody = <<"<letter subject=\"rt2\" author=\"tester\" type=\"bulletin\">\n"
                "  <predicate action_type=\"preposition\" action=\"if:actor\">\n"
                "    <actor>admin</actor>\n"
                "  </predicate>\n"
                "</letter>">>,
    Letter  = xml_letter:from_binary(XmlBody),
    JsonBin = http_herald:to_binary(Letter),
    Map     = jsone:decode(JsonBin, [{object_format, map}]),
    ?assertEqual(<<"rt2">>,     maps:get(<<"subject">>, Map)),
    ?assertEqual(<<"bulletin">>, maps:get(<<"type">>, Map)),
    [PMap]  = maps:get(<<"predicates">>, Map),
    ?assertEqual(<<"if:actor">>,    maps:get(<<"action">>, PMap)),
    ?assertEqual(<<"preposition">>, maps:get(<<"action_type">>, PMap)).

xml_content_type_header(_Config) ->
    %% Explicitly send Content-Type: application/xml
    XmlBody = <<"<letter subject=\"ct-xml\" author=\"test\" type=\"decree\">\n"
                "  <predicate action_type=\"verb\" action=\"do:notify\">\n"
                "    <channel>log</channel>\n"
                "  </predicate>\n"
                "</letter>">>,
    R = httpc:request(post,
            {api("/recite"),
             [{"x-api-key", ?API_KEY}, {"content-type", "application/xml"}],
             "application/xml", XmlBody},
            [{timeout, 5000}], [{body_format, binary}]),
    ?assertEqual(202, ct_helper:status(R)).

xml_accept_header(_Config) ->
    %% Send JSON, request XML response — letter-carrying endpoints return
    %% a simple status map so we just verify 202 and valid response.
    JsonBody = ct_helper:make_letter(uid(<<"accept-xml">>), []),
    R = httpc:request(post,
            {api("/recite"),
             [{"x-api-key", ?API_KEY}, {"accept", "application/xml"}],
             "application/json", JsonBody},
            [{timeout, 5000}], [{body_format, binary}]),
    ?assertEqual(202, ct_helper:status(R)).

xml_nested_abstract(_Config) ->
    %% A <predicate> whose <abstract> contains a nested <letter>
    XmlBody = <<"<letter subject=\"parent\" author=\"system\" type=\"decree\">\n"
                "  <predicate action_type=\"verb\" action=\"dispatch\">\n"
                "    <abstract>\n"
                "      <letter subject=\"child\" author=\"system\" type=\"decree\">\n"
                "        <predicate action_type=\"verb\" action=\"do:notify\">\n"
                "          <channel>ops</channel>\n"
                "          <level>critical</level>\n"
                "        </predicate>\n"
                "      </letter>\n"
                "    </abstract>\n"
                "  </predicate>\n"
                "</letter>">>,
    %% Verify xml_letter parses it correctly
    Letter = xml_letter:from_binary(XmlBody),
    ?assertMatch(#letter{subject = <<"parent">>}, Letter),
    [P] = Letter#letter.predicates,
    ?assertEqual({verb, <<"dispatch">>}, P#predicate.action),
    ?assertNotEqual(undefined, P#predicate.abstract),
    %% The abstract should be a JSON-encoded sub-letter
    SubLetter = http_herald:letter_from_binary(P#predicate.abstract),
    ?assertMatch(#letter{subject = <<"child">>}, SubLetter),
    %% Now recite it via HTTP
    R = xml_post(api("/recite"), XmlBody),
    ?assertEqual(202, ct_helper:status(R)).

%% ---------------------------------------------------------------------------
%% Internal helpers
%% ---------------------------------------------------------------------------

%% Post XML body with API key auth
xml_post(Url, XmlBody) ->
    httpc:request(post,
        {Url, [{"x-api-key", ?API_KEY}], "application/xml", XmlBody},
        [{timeout, 5000}], [{body_format, binary}]).

url(Path) ->
    "http://localhost:" ++ integer_to_list(?PORT) ++ Path.

api(Path) ->
    url("/api/v1" ++ Path).

uid(Prefix) ->
    ct_helper:unique_id(Prefix).

simple_letter() ->
    ct_helper:make_letter(<<"test-subject">>,
                          [ct_helper:make_verb(<<"do:notify">>, #{<<"to">> => <<"ops">>})]).

letter(Subject, Predicates) ->
    ct_helper:make_letter(Subject, Predicates).

verb(Action, Adjectives) ->
    ct_helper:make_verb(Action, Adjectives).

prep(Action, Adjectives) ->
    ct_helper:make_prep(Action, Adjectives).

webhook_reg(Id, Secret) ->
    Base = #{
        <<"id">>     => Id,
        <<"letter">> => #{
            <<"subject">>    => <<"inbound-event">>,
            <<"author">>     => <<"webhook">>,
            <<"type">>       => <<"decree">>,
            <<"predicates">> => [
                #{<<"action_type">> => <<"verb">>,
                  <<"action">>      => <<"do:notify">>,
                  <<"adjectives">>  => #{<<"channel">> => <<"log">>}}
            ]
        }
    },
    case Secret of
        undefined -> Base;
        S         -> Base#{<<"secret">> => S}
    end.
