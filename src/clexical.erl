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
    fresh_id/0,
    pronounce/2,
    hear/2,
    proclaim/2,
    compose_key/1,
    get_option/3,
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
    fill_ids/2
    ]).

start_link(Herald, Scribe, Vassal) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Herald, Scribe, Vassal], []).

stop() ->
    gen_server:call(?MODULE, stop).

init([{Herald, HOpts}, {Scribe, SOpts}, {Vassal, VOpts}]) ->
    lager:info(?LOGO,[]),
    Herald:init(HOpts),
    Scribe:init(SOpts),
    Vassal:init(VOpts),    
    {ok, #state{herald=Herald, scribe=Scribe, vassal=Vassal}}.

handle_info(Record, State) ->
    lager:debug("Unknown Info Request: ~p~n", [Record]),
    {noreply, State}.

handle_cast({recite, #letter{}=Letter},#state{herald=Herald}=State) ->
    lager:debug("Recite Letter: ~p~n", [Herald:to_binary(Letter)]),
    spawn(?MODULE, pronounce, [Letter, State]),
    {noreply, State};
handle_cast({attend, #letter{}=Letter}, #state{herald=Herald}=State) ->
    lager:debug("Hear Letter: ~p~n", [Herald:to_binary(Letter)]),
    spawn(?MODULE, hear, [Letter, State]),
    {noreply, State};    
handle_cast({proclaim, #letter{}=Letter}, #state{herald=Herald}=State) ->
    lager:debug("Proclaim Letter: ~p~n", [Herald:to_binary(Letter)]),
    spawn(?MODULE, proclaim, [Letter, State]),
    {noreply, State};
handle_cast(_Msg, State) ->
    lager:debug("Received Cast: ~p~n", [_Msg]),
    {noreply, State}.

handle_call(fresh_id, _From, #state{lastid=LID}=State) ->
    ID = LID + 1,
    {reply, erlang:integer_to_binary(ID), State#state{lastid=ID}};
handle_call(Info, _From, _State) ->
    lager:info("Received Call: ~p~n", [Info]),
    {reply, ok, _State}.

terminate(_Reason, _State) ->
    lager:info("Terminated Component.", []),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Clexical Functions

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
pronounce(#letter{predicates=[#predicate{action={preposition,_}}=P|T]}=Letter,  #state{last_predicate=LP}=State) ->
    PP= fill_id(P, LP),
    refrain(Letter#letter{predicates=[PP]}, State),
    pronounce(Letter#letter{predicates=T}, State);
pronounce(#letter{predicates=[#predicate{action={verb,_}}=P|T]}=Letter, State) ->    
    PP = fill_id(P),
    ID = PP#predicate.id,
    register(binary_to_atom(<<"work_", ID/binary>>, utf8), spawn(?MODULE, say, [Letter#letter{predicates=[PP]}, State])),
    pronounce(Letter#letter{predicates=T}, State);
pronounce(_, _) ->
    ok. % Empty Minded

-spec hear(#letter{}, #state{}) -> any().
hear(#letter{predicates=[#predicate{action={preposition,_}}=P|T]}=Letter, #state{scribe=Scribe, herald=Herald}=State) ->
    case Scribe:recall(compose_key(P)) of
        #predicate{}=PP ->
            ok;
        _ ->
            PP = Scribe:recall(compose_key(P#predicate{adjectives=[]}))
    end,
    lager:info("Recall: ~p~n", [PP]),
    pronounce(Letter#letter{predicates=Herald:excerpts(PP)}, State#state{last_predicate=P}),
    hear(Letter#letter{predicates=T}, State);
hear(_, _) ->    
    ok. % We don't take actions based on what we hear

% King's Functions

-spec say(#letter{}, #state{}) -> any().
say(#letter{predicates=[#predicate{}=P|_]}=Letter, #state{herald=Herald, vassal=Vassal, last_predicate=LP}=State) ->
    lager:info("Say: ~p~n", [Herald:to_binary(Letter)]),
    pronounce(Letter#letter{predicates=Herald:excerpts(P)}, State#state{last_predicate=P}),
    Reply = Vassal:work(Letter#letter{predicates=[P]}, LP),
    hear(Reply, State),
    proclaim(Reply, State);
say(_,_) ->
    ok.

-spec refrain(#letter{}, #state{}) -> any().
refrain(#letter{predicates=[#predicate{}=P|_]}=Letter, #state{herald=Herald, scribe=Scribe}=_State) ->
    Key = compose_key(P),
    lager:info("Refrain[~p]: ~p ~n", [Key, Herald:to_binary(Letter)]),    
    Scribe:curb(Key, P);
refrain(_,_) ->
    ok.

-spec proclaim(#letter{}, #state{}) -> any().
proclaim(#letter{predicates=[#predicate{}|_]}=Letter, #state{herald=Herald}) ->
    lager:info("Proclaim: ~p ~n", [Herald:to_binary(Letter)]),
    Herald:proclaim(Letter);
proclaim(_, _) ->    
    ok. % We don't take actions based on what we don't know

% Utils Functions

-spec fill_ids([#predicate{}], #predicate{}) -> [#predicate{}]|[].
fill_ids(PS, LP) ->
    lists:map(fun(P) -> fill_id(P, LP) end, PS).

-spec fill_id(#predicate{}) -> #predicate{}.
fill_id(#predicate{id= <<>>}=P) ->
    P#predicate{id=fresh_id()};
fill_id(#predicate{}=P) ->
    P.

-spec fill_id(#predicate{}, #predicate{}|undefined) -> #predicate{}.
fill_id(#predicate{id= <<>>}=P, undefined) ->
    P#predicate{id=fresh_id()};
fill_id(#predicate{id= <<>>, subject= <<>>}=P, #predicate{id=ID, subject=Subject}) ->
    fill_id(P#predicate{id=ID, subject=Subject});
fill_id(#predicate{id= <<>>}=P, #predicate{id=ID}) ->
    fill_id(P#predicate{id=ID});
fill_id(#predicate{}=P, _) ->
    P.

-spec fresh_id() -> binary().
fresh_id() ->
    gen_server:call(clexical, fresh_id).

-spec compose_key(#predicate{}) -> binary().
compose_key(#predicate{adjectives={dict, _, _, _, _, _, _, _, _}=Dict}=P) ->
    BareKey = compose_key(P#predicate{adjectives=[]}),
    Suffix = dict:fold(fun(_K, [V], A) -> <<A/binary, V/binary>> end, <<>>, Dict),
    <<BareKey/binary, Suffix/binary>>;
compose_key(#predicate{action={_,BName}, subject=Subject, id=ID}) ->
    <<Subject/binary, ID/binary, BName/binary>>;
compose_key(_) ->
    <<>>.    

get_option(Key, Opts, Default) ->
    case lists:keyfind(Key, 1, Opts) of
        {_K, V} ->
            V;
        _ ->
            Default
    end.

-spec get_adjective(binary(), dict()) -> binary() | undefined.
get_adjective(Key, Dict) ->
    get_adjective(Key, Dict, undefined).

-spec get_adjective(binary(), dict(), any()) -> binary() | undefined.
get_adjective(Key, Dict, Default) ->
    case dict:is_key(Key, Dict) of
        true ->
            [V] = dict:fetch(Key, Dict);
        _ -> 
            V = Default
    end,
    V.
