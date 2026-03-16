%%
%% clexical_stress_SUITE — Performance benchmark suite.
%%
%% Run via:  rebar3 as test ct --suite clexical_stress_SUITE
%% Or via Docker: ./run_tests.sh --stress
%%
%% Environment variables:
%%   STRESS_N  — requests per worker (default 200)
%%   STRESS_C  — concurrent workers  (default 20)
%%
%% Each benchmark prints:
%%   Total requests, Duration (ms), Throughput (req/s),
%%   Latency p50/p95/p99 (ms), Error count
%%
-module(clexical_stress_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-define(PORT,    18099).
-define(API_KEY, "stress-test-key").
-define(KEY,     <<"stress-test-key">>).

-export([all/0, suite/0, init_per_suite/1, end_per_suite/1]).
-export([
    benchmark_recite_single_worker/1,
    benchmark_recite_concurrent/1,
    benchmark_attend_throughput/1,
    benchmark_automations_register/1,
    benchmark_webhook_inbound/1
]).

suite() -> [{timetrap, {minutes, 5}}].

all() ->
    [
        benchmark_recite_single_worker,
        benchmark_recite_concurrent,
        benchmark_attend_throughput,
        benchmark_automations_register,
        benchmark_webhook_inbound
    ].

init_per_suite(Config) ->
    ok = ct_helper:start_app(?PORT, ?API_KEY),
    N = env_int("STRESS_N", 200),
    C = env_int("STRESS_C", 20),
    ct:pal("~n~n===================================================="),
    ct:pal(" STRESS TEST  N=~p requests/worker  C=~p workers", [N, C]),
    ct:pal("====================================================~n"),
    [{n, N}, {c, C} | Config].

end_per_suite(_Config) ->
    ct_helper:stop_app().

%% ---------------------------------------------------------------------------
%% Benchmarks
%% ---------------------------------------------------------------------------

benchmark_recite_single_worker(Config) ->
    N = ?config(n, Config),
    ct:pal(">>> benchmark_recite_single_worker  N=~p", [N]),
    {Duration, Latencies, Errors} = run_sequential(N, fun recite_job/0),
    print_report("recite/sequential", N, Duration, Latencies, Errors),
    assert_error_rate(Errors, N, 0.02).  %% max 2% error rate

benchmark_recite_concurrent(Config) ->
    N = ?config(n, Config),
    C = ?config(c, Config),
    ct:pal(">>> benchmark_recite_concurrent  N=~p  C=~p", [N, C]),
    {Duration, Latencies, Errors} = run_concurrent(C, N, fun recite_job/0),
    print_report("recite/concurrent", N * C, Duration, Latencies, Errors),
    assert_error_rate(Errors, N * C, 0.05).  %% max 5% error rate under load

benchmark_attend_throughput(Config) ->
    N = ?config(n, Config),
    C = min(?config(c, Config), 10),  %% attend also writes to mnesia
    ct:pal(">>> benchmark_attend_throughput  N=~p  C=~p", [N, C]),
    {Duration, Latencies, Errors} = run_concurrent(C, N, fun attend_job/0),
    print_report("attend/concurrent", N * C, Duration, Latencies, Errors),
    assert_error_rate(Errors, N * C, 0.05).

benchmark_automations_register(Config) ->
    N = min(?config(n, Config), 100),
    ct:pal(">>> benchmark_automations_register  N=~p", [N]),
    {Duration, Latencies, Errors} = run_sequential(N, fun register_automation_job/0),
    print_report("automations/register", N, Duration, Latencies, Errors),
    assert_error_rate(Errors, N, 0.0).  %% zero tolerance

benchmark_webhook_inbound(Config) ->
    N = ?config(n, Config),
    C = ?config(c, Config),
    ct:pal(">>> benchmark_webhook_inbound  N=~p  C=~p", [N, C]),
    %% Register a webhook endpoint first
    Id = <<"stress-inbound-hook">>,
    Reg = #{<<"id">> => Id,
            <<"letter">> => #{<<"subject">> => <<"stress-event">>,
                              <<"author">>  => <<"webhook">>,
                              <<"type">>    => <<"decree">>,
                              <<"predicates">> => []}},
    ct_helper:http_post(api("/webhooks"), ?KEY, jsone:encode(Reg)),
    InUrl = api("/webhooks/in/" ++ binary_to_list(Id)),
    Job = fun() ->
        Payload = jsone:encode(#{<<"ts">> => erlang:system_time(millisecond)}),
        ct_helper:http_no_auth_post(InUrl, Payload)
    end,
    {Duration, Latencies, Errors} = run_concurrent(C, N, Job),
    print_report("webhook/inbound", N * C, Duration, Latencies, Errors),
    assert_error_rate(Errors, N * C, 0.05).

%% ---------------------------------------------------------------------------
%% Workload runners
%% ---------------------------------------------------------------------------

%% Run N iterations sequentially, return {WallMs, [LatencyMs], ErrorCount}.
run_sequential(N, Job) ->
    Start = erlang:monotonic_time(millisecond),
    {Latencies, Errors} = lists:foldl(fun(_, {Lats, Errs}) ->
        T0 = erlang:monotonic_time(millisecond),
        Res = (catch Job()),
        T1 = erlang:monotonic_time(millisecond),
        Lat = T1 - T0,
        case is_success(Res) of
            true  -> {[Lat | Lats], Errs};
            false -> {[Lat | Lats], Errs + 1}
        end
    end, {[], 0}, lists:seq(1, N)),
    End = erlang:monotonic_time(millisecond),
    {End - Start, Latencies, Errors}.

%% Run C workers each doing N iterations in parallel.
run_concurrent(C, N, Job) ->
    Parent = self(),
    Start  = erlang:monotonic_time(millisecond),
    Workers = [spawn_monitor(fun() ->
        Result = run_sequential(N, Job),
        Parent ! {worker_done, self(), Result}
    end) || _ <- lists:seq(1, C)],
    Results = [receive
        {worker_done, Pid, R} -> R
    after 240_000 -> {0, [], N}  %% timeout counts all as errors
    end || {Pid, _Ref} <- Workers],
    End = erlang:monotonic_time(millisecond),
    AllLats  = lists:flatten([Lats  || {_, Lats, _}  <- Results]),
    AllErrs  = lists:sum([Errs  || {_, _, Errs}  <- Results]),
    {End - Start, AllLats, AllErrs}.

%% ---------------------------------------------------------------------------
%% Job factories
%% ---------------------------------------------------------------------------

recite_job() ->
    Id   = integer_to_binary(erlang:unique_integer([positive, monotonic])),
    Body = ct_helper:make_letter(
               <<"stress-", Id/binary>>,
               [ct_helper:make_verb(<<"do:notify">>,
                                    #{<<"seq">> => Id, <<"ts">> => erlang:system_time(millisecond)})]),
    ct_helper:http_post(api("/recite"), ?KEY, Body).

attend_job() ->
    Id   = integer_to_binary(erlang:unique_integer([positive, monotonic])),
    Body = ct_helper:make_letter(
               <<"stress-attend-", Id/binary>>,
               [ct_helper:make_prep(<<"store:event">>,
                                    #{<<"id">> => Id, <<"type">> => <<"load_test">>})]),
    ct_helper:http_post(api("/attend"), ?KEY, Body).

register_automation_job() ->
    Id = ct_helper:unique_id(<<"stress-auto">>),
    Auto = #{
        <<"id">>       => Id,
        <<"name">>     => <<"Stress ", Id/binary>>,
        <<"schedule">> => #{<<"type">> => <<"interval">>, <<"ms">> => 999999},
        <<"letter">>   => #{<<"subject">> => <<"stress">>, <<"author">> => <<"test">>,
                            <<"type">> => <<"decree">>, <<"predicates">> => []}
    },
    ct_helper:http_post(api("/automations"), ?KEY, jsone:encode(Auto)).

%% ---------------------------------------------------------------------------
%% Result analysis + reporting
%% ---------------------------------------------------------------------------

is_success({ok, {{_, S, _}, _, _}}) when S >= 200, S < 300 -> true;
is_success({ok, {{_, S, _}, _, _}}) when S =:= 202          -> true;
is_success(_)                                                -> false.

print_report(Name, Total, DurationMs, Latencies, Errors) ->
    Sorted = lists:sort(Latencies),
    P50    = percentile(Sorted, 0.50),
    P95    = percentile(Sorted, 0.95),
    P99    = percentile(Sorted, 0.99),
    Tps    = case DurationMs of
                 0 -> Total * 1000;
                 D -> trunc(Total * 1000 / D)
             end,
    Pct    = case Total of
                 0 -> 0.0;
                 _ -> Errors * 100.0 / Total
             end,
    ct:pal("~n────────────────────────────────────────────────────"),
    ct:pal(" ~s", [Name]),
    ct:pal("  Total:      ~p requests", [Total]),
    ct:pal("  Duration:   ~p ms", [DurationMs]),
    ct:pal("  Throughput: ~p req/s", [Tps]),
    ct:pal("  Latency p50:~p ms  p95:~p ms  p99:~p ms",
           [P50, P95, P99]),
    ct:pal("  Errors:     ~p (~.1f%)", [Errors, Pct]),
    ct:pal("────────────────────────────────────────────────────~n").

percentile([], _P)    -> 0;
percentile(Sorted, P) ->
    Idx = max(1, round(P * length(Sorted))),
    lists:nth(min(Idx, length(Sorted)), Sorted).

assert_error_rate(Errors, Total, MaxRate) ->
    ActualRate = case Total of
        0 -> 0.0;
        _ -> Errors / Total
    end,
    ?assert(ActualRate =< MaxRate).

%% ---------------------------------------------------------------------------
%% Helpers
%% ---------------------------------------------------------------------------

url(Path) ->
    "http://localhost:" ++ integer_to_list(?PORT) ++ Path.

api(Path) ->
    url("/api/v1" ++ Path).

env_int(Var, Default) ->
    case os:getenv(Var) of
        false -> Default;
        S     -> list_to_integer(S)
    end.
