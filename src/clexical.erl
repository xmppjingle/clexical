-module(clexical).
-behaviour(gen_server).

-include("../include/clexical.hrl").
-include("../include/clexical_app.hrl").

%% gen_server callbacks
-export([
    start_link/3,
    stop/0,
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

%% API Functions
-export([
    pronounce/2,
    hear/2,
    proclaim/2,
    compose_key/1,
    proclaim/1,
    recite/1,
    attend/1,
    say/2
]).

-export([
    get_adjective/3,
    get_adjective/2,
    fill_id/1,
    fill_id/2,
    fill_ids/2,
    fill_subject/2,
    fill_subjects/2
    ]).

start_link(Herald, Scribe, Vassal) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Herald, Scribe, Vassal], []).

stop() ->
    gen_server:call(?MODULE, stop).

init([{Herald, HOpts}, {Scribe, SOpts}, {Vassal, VOpts}]) ->
    logger:info(?LOGO),
    Herald:initialize(HOpts),
    Scribe:initialize(SOpts),
    Vassal:initialize(VOpts),
    S = #state{herald=Herald, scribe=Scribe, vassal=Vassal},
    logger:info("Clexical Started with: ~p", [S]),
    {ok, S}.

handle_info(Record, State) ->
    logger:debug("Unknown Info Request: ~p", [Record]),
    {noreply, State}.

handle_cast({recite, #letter{}=Letter},#state{herald=Herald}=State) ->
    logger:debug("Recite Letter: ~p", [Herald:to_binary(Letter)]),
    spawn_monitor(?MODULE, pronounce, [Letter, State]),
    {noreply, State};
handle_cast({attend, #letter{}=Letter}, #state{herald=Herald}=State) ->
    logger:debug("Hear Letter: ~p", [Herald:to_binary(Letter)]),
    spawn_monitor(?MODULE, hear, [Letter, State]),
    spawn_monitor(?MODULE, proclaim, [Letter, State]),
    {noreply, State};
handle_cast({proclaim, #letter{}=Letter}, #state{herald=Herald}=State) ->
    logger:debug("Proclaim Letter: ~p", [Herald:to_binary(Letter)]),
    spawn_monitor(?MODULE, proclaim, [Letter, State]),
    {noreply, State};
handle_cast(_Msg, State) ->
    logger:debug("Received Cast: ~p", [_Msg]),
    {noreply, State}.

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(Info, _From, State) ->
    logger:info("Received Call: ~p", [Info]),
    {reply, ok, State}.

terminate(_, _) ->
    logger:debug("Terminating: ~p", [?MODULE]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ---------------------------------------------------------------------------
%% Clexical Functions
%% ---------------------------------------------------------------------------

-spec recite(#letter{}) -> any().
recite(#letter{}=L) ->
    gen_server:cast(clexical, {recite, L}).

-spec attend(#letter{}) -> any().
attend(#letter{}=L) ->
    gen_server:cast(clexical, {attend, L}).

-spec proclaim(#letter{}) -> any().
proclaim(#letter{}=L) ->
    gen_server:cast(clexical, {proclaim, L}).

-spec pronounce(#letter{}, #state{}) -> any().
pronounce(#letter{predicates=[#predicate{action = {preposition,_}} = P|T]} = Letter,  #state{last_predicate = LP} = State) ->
    PP = fill_id(P, LP),
    refrain(Letter#letter{predicates=[PP]}, State),
    pronounce(Letter#letter{predicates=T}, State);
pronounce(#letter{predicates=[#predicate{action = {verb,_}} = P|T]} = Letter, #state{last_predicate = LP} = State) ->
    PP = fill_subject(fill_id(P), LP),
    {_PID, _Ref} = spawn_monitor(?MODULE, say, [Letter#letter{predicates=[PP]}, State]),
    pronounce(Letter#letter{predicates=T}, State#state{last_predicate=PP});
pronounce(#letter{predicates=[]} = Letter, #state{last_predicate = _LP} = State) ->
    {_PID, _Ref} = spawn_monitor(?MODULE, say, [Letter, State]);
pronounce(_, _) ->
    ok. % Empty Minded

-spec hear(#letter{}, #state{}) -> any().
hear(#letter{predicates=[#predicate{action={preposition,_}}=P|T]}=Letter, #state{scribe=Scribe, herald=Herald}=State) ->
    Key = compose_key(P),
    PP = case Scribe:recall(Key) of
        #predicate{} = Found ->
            Found;
        _ ->
            case Scribe:recall(compose_key(P#predicate{adjectives = #{}})) of
                #predicate{} = Found2 ->
                    Found2;
                _ ->
                    case Scribe:recall(compose_key(P#predicate{id = ?ANY_ID, subject = ?ANY_SUBJECT, adjectives = #{}})) of
                        #predicate{} = Found3 -> Found3;
                        Other -> Other
                    end
            end
    end,
    logger:debug("Recall[~p]: ~p", [Key, PP]),
    pronounce(Letter#letter{predicates=Herald:excerpts(PP)}, State#state{last_predicate=P}),
    hear(Letter#letter{predicates=T}, State);
hear(#letter{predicates=[#predicate{action={verb,_}}|T]}=Letter, #state{}=State) ->
    hear(Letter#letter{predicates=T}, State);
hear(_, _) ->
    ok. % We don't take actions based on what we hear

-spec say(#letter{}, #state{}) -> any().
say(#letter{predicates=[#predicate{}=P|_]}=Letter, #state{herald=Herald, vassal=Vassal, last_predicate=LP}=State) ->
    logger:debug("Say: ~p", [Letter]),
    pronounce(Letter#letter{predicates=Herald:excerpts(P)}, State#state{last_predicate=P}),
    Reply = Vassal:work(Letter#letter{predicates=[P]}, LP),
    hear(Reply, State),
    proclaim(Reply, State);
say(#letter{predicates=[]}=Letter, #state{vassal=Vassal, last_predicate=LP}=State) ->
    logger:debug("Say: ~p", [Letter]),
    Reply = Vassal:work(Letter, LP),
    hear(Reply, State),
    proclaim(Reply, State);
say(_,_) ->
    ok.

-spec refrain(#letter{}, #state{}) -> any().
refrain(#letter{predicates=[#predicate{}=P|_]}=Letter, #state{herald=Herald, scribe=Scribe}=_State) ->
    Key = compose_key(P),
    logger:debug("Refrain[~p]: ~p", [Key, Herald:to_binary(Letter)]),
    Scribe:curb(Key, P);
refrain(_,_) ->
    ok.

-spec proclaim(#letter{}, #state{}) -> any().
proclaim(#letter{predicates=[#predicate{}|_]}=Letter, #state{herald=Herald}) ->
    logger:debug("Proclaim: ~p", [Herald:to_binary(Letter)]),
    Herald:proclaim(Letter);
proclaim(_, _) ->
    ok.

%% ---------------------------------------------------------------------------
%% Utility Functions
%% ---------------------------------------------------------------------------

-spec fill_subjects([#predicate{}], #predicate{}) -> [#predicate{}]|[].
fill_subjects(PS, LP) ->
    lists:map(fun(P) -> fill_subject(P, LP) end, PS).

fill_subject(#predicate{}=P, #predicate{subject = ParentSubject}) ->
    fill_subject(P, ParentSubject);
fill_subject(#predicate{subject = Subject}=P, ParentSubject) when Subject == ?ANY_SUBJECT; Subject == <<>>; Subject == undefined; Subject == false ->
    P#predicate{subject = ParentSubject};
fill_subject(#predicate{}=P, _) ->
    P.

-spec fill_ids([#predicate{}], #predicate{}) -> [#predicate{}]|[].
fill_ids(PS, LP) ->
    lists:map(fun(P) -> fill_id(P, LP) end, PS).

-spec fill_id(#predicate{}) -> #predicate{}.
fill_id(#predicate{id = ID} = P) when ID == <<>>; ID == ?ANY_ID; ID == undefined; ID == false ->
    P#predicate{id=clexical_id:fresh_id()};
fill_id(#predicate{}=P) ->
    P.

-spec fill_id(#predicate{}, #predicate{}|undefined) -> #predicate{}.
fill_id(#predicate{id = ID, action = {verb, _}}=P, undefined) when ID == <<>>; ID == ?ANY_ID; ID == undefined; ID == false ->
    P#predicate{id=clexical_id:fresh_id()};
fill_id(#predicate{id = <<>>, subject= <<>>}=P, #predicate{id=ID, subject=Subject}) ->
    fill_id(P#predicate{id=ID, subject=Subject});
fill_id(#predicate{id = ID}=P, #predicate{id=PID}) when ID == <<>>; ID == ?ANY_ID; ID == undefined; ID == false ->
    fill_id(P#predicate{id=PID});
fill_id(#predicate{}=P, _) ->
    P.

-spec compose_key(#predicate{}) -> binary().
compose_key(#predicate{adjectives = #{} = Map} = P) when map_size(Map) > 0 ->
    BareKey = compose_key(P#predicate{adjectives=#{}}),
    Suffix = maps:fold(fun(_K, V, A) -> <<A/binary, V/binary>> end, <<>>,
                 maps:filter(fun(K, _) -> K /= <<"id">> andalso K /= <<"subject">> end, Map)),
    <<BareKey/binary, Suffix/binary>>;
compose_key(#predicate{action={_,BName}, subject=Subject, id=ID}) ->
    <<Subject/binary, ".", ID/binary, BName/binary>>;
compose_key(_) ->
    <<>>.

-spec get_adjective(binary(), map()) -> binary() | undefined.
get_adjective(Key, Map) ->
    get_adjective(Key, Map, undefined).

-spec get_adjective(binary(), map(), any()) -> binary() | undefined.
get_adjective(Key, Map, Default) ->
    maps:get(Key, Map, Default).
