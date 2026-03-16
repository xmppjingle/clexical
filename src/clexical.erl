%%
%% clexical — core event-driven evaluator engine.
%%
%% This gen_server is the heart of the Clexical paradigm.
%% It dispatches #letter{} messages through three phases:
%%
%%   pronounce — for each predicate, either store (preposition) or execute (verb)
%%   hear      — recall stored predicates and pronounce their sub-clauses
%%   proclaim  — broadcast results back through the Herald
%%
%% Each phase spawns monitored worker processes so all predicates
%% in a letter are processed concurrently.
%%
%% Pluggable behaviours:
%%   Herald  — protocol/transport layer (http_herald, xmpp_herald, …)
%%   Scribe  — storage layer            (mnesia_scribe, …)
%%   Vassal  — work/execution layer     (automation_vassal, …)
%%

-module(clexical).
-behaviour(gen_server).

-include("../include/clexical.hrl").
-include("../include/clexical_app.hrl").

%% gen_server callbacks
-export([start_link/3, stop/0,
         init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

%% Public dispatch API
-export([recite/1, attend/1, proclaim/1]).

%% Internal (called via spawn_monitor)
-export([pronounce/2, hear/2, proclaim/2, say/2]).

%% Utility helpers used by handlers and vassal implementations
-export([compose_key/1,
         get_adjective/2, get_adjective/3,
         fill_id/1, fill_id/2, fill_ids/2,
         fill_subject/2, fill_subjects/2]).

%% ---------------------------------------------------------------------------
%% Start / Stop
%% ---------------------------------------------------------------------------

%% Herald and Vassal are already running (started by clexical_services_sup).
%% We receive just their module names plus the Scribe spec so we can
%% initialise Mnesia tables before the first message arrives.
start_link(Herald, {Scribe, SOpts}, Vassal) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE,
                          {Herald, {Scribe, SOpts}, Vassal}, []).

stop() ->
    gen_server:call(?MODULE, stop).

init({Herald, {Scribe, SOpts}, Vassal}) ->
    logger:info(?LOGO),
    %% Only the Scribe needs explicit initialisation (Mnesia table setup).
    %% Herald and Vassal are already supervised and started.
    Scribe:initialize(SOpts),
    S = #state{herald = Herald, scribe = Scribe, vassal = Vassal},
    logger:info("clexical started — herald=~p scribe=~p vassal=~p",
                [Herald, Scribe, Vassal]),
    {ok, S}.

handle_cast({recite, #letter{} = Letter}, #state{herald = Herald} = State) ->
    logger:debug("recite: ~p", [Herald:to_binary(Letter)]),
    spawn_monitor(?MODULE, pronounce, [Letter, State]),
    {noreply, State};

handle_cast({attend, #letter{} = Letter}, #state{herald = Herald} = State) ->
    logger:debug("attend: ~p", [Herald:to_binary(Letter)]),
    spawn_monitor(?MODULE, hear,     [Letter, State]),
    spawn_monitor(?MODULE, proclaim, [Letter, State]),
    {noreply, State};

handle_cast({proclaim, #letter{} = Letter}, #state{herald = Herald} = State) ->
    logger:debug("proclaim: ~p", [Herald:to_binary(Letter)]),
    spawn_monitor(?MODULE, proclaim, [Letter, State]),
    {noreply, State};

handle_cast(Msg, State) ->
    logger:debug("unhandled cast: ~p", [Msg]),
    {noreply, State}.

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(Call, _From, State) ->
    logger:info("unhandled call: ~p", [Call]),
    {reply, ok, State}.

handle_info(Info, State) ->
    logger:debug("unhandled info: ~p", [Info]),
    {noreply, State}.

terminate(_, _) ->
    logger:debug("clexical terminating"),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ---------------------------------------------------------------------------
%% Public dispatch API
%% ---------------------------------------------------------------------------

-spec recite(#letter{}) -> ok.
recite(#letter{} = L) ->
    gen_server:cast(clexical, {recite, L}).

-spec attend(#letter{}) -> ok.
attend(#letter{} = L) ->
    gen_server:cast(clexical, {attend, L}).

-spec proclaim(#letter{}) -> ok.
proclaim(#letter{} = L) ->
    gen_server:cast(clexical, {proclaim, L}).

%% ---------------------------------------------------------------------------
%% Core dispatch phases
%% ---------------------------------------------------------------------------

%% pronounce/2 — walk the predicate list; store prepositions, spawn verbs.
-spec pronounce(#letter{}, #state{}) -> ok.
pronounce(#letter{predicates = [#predicate{action = {preposition, _}} = P | T]} = Letter,
          #state{last_predicate = LP} = State) ->
    PP = fill_id(P, LP),
    refrain(Letter#letter{predicates = [PP]}, State),
    pronounce(Letter#letter{predicates = T}, State);

pronounce(#letter{predicates = [#predicate{action = {verb, _}} = P | T]} = Letter,
          #state{last_predicate = LP} = State) ->
    PP = fill_subject(fill_id(P), LP),
    spawn_monitor(?MODULE, say, [Letter#letter{predicates = [PP]}, State]),
    pronounce(Letter#letter{predicates = T}, State#state{last_predicate = PP});

pronounce(#letter{predicates = []} = Letter, State) ->
    spawn_monitor(?MODULE, say, [Letter, State]),
    ok;

pronounce(_, _) ->
    ok.

%% hear/2 — for each preposition, recall from Scribe and pronounce the result.
-spec hear(#letter{}, #state{}) -> ok.
hear(#letter{predicates = [#predicate{action = {preposition, _}} = P | T]} = Letter,
     #state{scribe = Scribe, herald = Herald} = State) ->
    Key = compose_key(P),
    PP  = recall_with_fallback(Scribe, Key, P),
    logger:debug("hear recall[~p]: ~p", [Key, PP]),
    pronounce(Letter#letter{predicates = Herald:excerpts(PP)},
              State#state{last_predicate = P}),
    hear(Letter#letter{predicates = T}, State);

hear(#letter{predicates = [#predicate{action = {verb, _}} | T]} = Letter, State) ->
    hear(Letter#letter{predicates = T}, State);

hear(_, _) ->
    ok.

%% say/2 — run vassal work for a verb predicate, then hear and proclaim reply.
-spec say(#letter{}, #state{}) -> ok.
say(#letter{predicates = [#predicate{} = P | _]} = Letter,
    #state{herald = Herald, vassal = Vassal, last_predicate = LP} = State) ->
    logger:debug("say: ~p", [Letter]),
    pronounce(Letter#letter{predicates = Herald:excerpts(P)},
              State#state{last_predicate = P}),
    Reply = Vassal:work(Letter#letter{predicates = [P]}, LP),
    hear(Reply, State),
    proclaim(Reply, State);

say(#letter{predicates = []} = Letter,
    #state{vassal = Vassal, last_predicate = LP} = State) ->
    logger:debug("say (empty): ~p", [Letter]),
    Reply = Vassal:work(Letter, LP),
    hear(Reply, State),
    proclaim(Reply, State);

say(_, _) ->
    ok.

%% refrain/2 — store a preposition predicate via the Scribe.
-spec refrain(#letter{}, #state{}) -> ok.
refrain(#letter{predicates = [#predicate{} = P | _]} = Letter,
        #state{herald = Herald, scribe = Scribe}) ->
    Key = compose_key(P),
    logger:debug("refrain[~p]: ~p", [Key, Herald:to_binary(Letter)]),
    Scribe:curb(Key, P),
    ok;
refrain(_, _) ->
    ok.

%% proclaim/2 — send results back through the Herald.
-spec proclaim(#letter{}, #state{}) -> ok.
proclaim(#letter{predicates = [#predicate{} | _]} = Letter, #state{herald = Herald}) ->
    logger:debug("proclaim: ~p", [Herald:to_binary(Letter)]),
    Herald:proclaim(Letter),
    ok;
proclaim(_, _) ->
    ok.

%% ---------------------------------------------------------------------------
%% Scribe recall with progressive key relaxation
%% ---------------------------------------------------------------------------

recall_with_fallback(Scribe, Key, P) ->
    case Scribe:recall(Key) of
        #predicate{} = Found -> Found;
        _ ->
            case Scribe:recall(compose_key(P#predicate{adjectives = #{}})) of
                #predicate{} = Found2 -> Found2;
                _ ->
                    Scribe:recall(compose_key(
                        P#predicate{id = ?ANY_ID, subject = ?ANY_SUBJECT,
                                    adjectives = #{}}))
            end
    end.

%% ---------------------------------------------------------------------------
%% Key composition
%% ---------------------------------------------------------------------------

-spec compose_key(#predicate{}) -> binary().
compose_key(#predicate{adjectives = Adjs} = P) when map_size(Adjs) > 0 ->
    Base   = compose_key(P#predicate{adjectives = #{}}),
    Suffix = maps:fold(
        fun(K, V, Acc) when K /= <<"id">>, K /= <<"subject">> ->
                <<Acc/binary, V/binary>>;
           (_, _, Acc) -> Acc
        end, <<>>, Adjs),
    <<Base/binary, Suffix/binary>>;
compose_key(#predicate{action = {_, BName}, subject = Subject, id = ID}) ->
    <<Subject/binary, ".", ID/binary, BName/binary>>;
compose_key(_) ->
    <<>>.

%% ---------------------------------------------------------------------------
%% ID and Subject filling utilities
%% ---------------------------------------------------------------------------

-spec fill_subjects([#predicate{}], #predicate{}) -> [#predicate{}].
fill_subjects(PS, LP) ->
    [fill_subject(P, LP) || P <- PS].

fill_subject(#predicate{} = P, #predicate{subject = PS}) ->
    fill_subject(P, PS);
fill_subject(#predicate{subject = S} = P, ParentS)
  when S =:= ?ANY_SUBJECT; S =:= <<>>; S =:= undefined; S =:= false ->
    P#predicate{subject = ParentS};
fill_subject(#predicate{} = P, _) ->
    P.

-spec fill_ids([#predicate{}], #predicate{}) -> [#predicate{}].
fill_ids(PS, LP) ->
    [fill_id(P, LP) || P <- PS].

-spec fill_id(#predicate{}) -> #predicate{}.
fill_id(#predicate{id = ID} = P)
  when ID =:= <<>>; ID =:= ?ANY_ID; ID =:= undefined; ID =:= false ->
    P#predicate{id = clexical_id:fresh_id()};
fill_id(#predicate{} = P) ->
    P.

-spec fill_id(#predicate{}, #predicate{} | undefined) -> #predicate{}.
fill_id(#predicate{id = ID, action = {verb, _}} = P, undefined)
  when ID =:= <<>>; ID =:= ?ANY_ID; ID =:= undefined; ID =:= false ->
    P#predicate{id = clexical_id:fresh_id()};
fill_id(#predicate{id = <<>>, subject = <<>>} = P, #predicate{id = ID, subject = S}) ->
    fill_id(P#predicate{id = ID, subject = S});
fill_id(#predicate{id = ID} = P, #predicate{id = PID})
  when ID =:= <<>>; ID =:= ?ANY_ID; ID =:= undefined; ID =:= false ->
    fill_id(P#predicate{id = PID});
fill_id(#predicate{} = P, _) ->
    P.

%% ---------------------------------------------------------------------------
%% Adjective accessors
%% ---------------------------------------------------------------------------

-spec get_adjective(binary(), map()) -> binary() | undefined.
get_adjective(Key, Map) ->
    get_adjective(Key, Map, undefined).

-spec get_adjective(binary(), map(), any()) -> any().
get_adjective(Key, Map, Default) ->
    maps:get(Key, Map, Default).
