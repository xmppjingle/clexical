-module(clexical).
-behaviour(gen_server).

-include("../include/clexical.hrl").

%% gen_server callbacks
-export([
    start_link/1,
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
    hear/2
]).

start_link(Herald) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Herald], []).

stop() ->
    gen_server:call(?MODULE, stop).

init([Herald]) ->
    lager:info(?LOGO,[]),
    {ok, #state{herald=Herald}}.

handle_info(Record, State) ->
    lager:debug("Unknown Info Request: ~p~n", [Record]),
    {noreply, State}.

handle_cast(_Msg, State) ->
    lager:debug("Received Cast: ~p~n", [_Msg]),
    {noreply, State}.

handle_call(fresh_id, _From, #state{lastid=LID}=State) ->
    ID = LID + 1,
    {reply, erlang:integer_to_binary(ID), State#state{lastid=ID}};
handle_call({recite, #letter{}=Letter}, _From, State) ->
    lager:info("Recite Letter: ~p~n", [Letter]),
    spawn(?MODULE, pronounce, [Letter, State]),
    {reply, ok, State};
handle_call({attend, #letter{}=Letter}, _From, State) ->
    lager:info("Hear Letter: ~p~n", [Letter]),
    spawn(?MODULE, hear, [Letter, State]),
    {reply, ok, State};
handle_call(Info, _From, _State) ->
    lager:info("Received Call: ~p~n", [Info]),
    {reply, ok, _State}.

terminate(_Reason, _State) ->
    lager:info("Terminated Component.", []),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Clexical Functions
-spec pronounce(#letter{}, #state{}) -> any().
pronounce(#letter{predicates=[#predicate{action={adverb,_}}=P|T]}=Letter,  #state{last_predicate=#predicate{id=ID, subject=Subject}}=State) ->
    refrain(P#predicate{id=ID, subject=Subject}, State),
    pronounce(Letter#letter{predicates=T}, State);
pronounce(#letter{predicates=[#predicate{action={verb,_}}=P|T]}=Letter, State) ->    
    say(P#predicate{id=fresh_id()}, State),
    pronounce(Letter#letter{predicates=T}, State);
pronounce(_, _) ->
    ok. % Empty Minded

-spec say(#predicate{}, #state{}) -> any().
say(#predicate{}=P, #state{herald=Herald}=State) ->
    lager:info("Say: ~p~n", [P]),
    pronounce(Herald:read_excerpt(P), State#state{last_predicate=P}),
    Herald:proclaim(P).

-spec refrain(#predicate{}, #state{}) -> any().
refrain(#predicate{}=P, #state{herald=Herald}=_State) ->
    lager:debug("Refrain: ~p ~n", [P]),
    Key = compose_key(P),
    Herald:curb(Key, P).

-spec hear(#letter{}, #state{}) -> any().
hear(#letter{predicates=[#predicate{action={adverb,_}}=P|T]}=Letter, #state{herald=Herald}=State) ->
    lager:debug("Hear: ~p ~n", [P]),
    Excerpt = Herald:read_excerpt(Herald:recall(compose_key(P))),
    pronounce(Excerpt, State),
    hear(Letter#letter{predicates=T}, State);
hear(_, _) ->    
    ok. % We don't take actions based on what we hear

% Utils Functions
-spec fresh_id() -> binary().
fresh_id() ->
    gen_server:call(clexical, fresh_id).

-spec compose_key(#predicate{}) -> binary().
compose_key(#predicate{adjectives={dict, _, _, _, _, _, _, _, _}=Dict}=P) ->
    BareKey = compose_key(P#predicate{adjectives=[]}),
    Suffix = dict:fold(fun(_K, V, A) -> A ++ V end, <<>>, Dict),
    <<BareKey/binary, Suffix/binary>>;
compose_key(#predicate{action={_,Name}, subject=Subject, id=ID}) ->
    BName=erlang:atom_to_binary(Name, utf8),
    <<Subject/binary, ID/binary, BName/binary>>;
compose_key(_) ->
    <<>>.    
