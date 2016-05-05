-module(clexical).
-behaviour(gen_server).

-include("../include/clexical.hrl").

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
    get_option/3
]).

start_link(Herald, Scribe, Vassal) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Herald, Scribe, Vassal], []).

stop() ->
    gen_server:call(?MODULE, stop).

init([Herald, Scribe, Vassal]) ->
    lager:info(?LOGO,[]),
    {ok, #state{herald=Herald, scribe=Scribe, vassal=Vassal}}.

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
handle_call({proclaim, #letter{}=Letter}, _From, State) ->
    lager:info("Proclaim Letter: ~p~n", [Letter]),
    spawn(?MODULE, proclaim, [Letter, State]),
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
pronounce(#letter{predicates=[#predicate{action={adverb,_}}=P|T]}=Letter,  #state{last_predicate=LP}=State) ->
    case LP of
        #predicate{id=ID, subject=Subject} ->
            refrain(P#predicate{id=ID, subject=Subject}, State);
        _ ->
            refrain(P#predicate{id=fresh_id()}, State)
    end,
    pronounce(Letter#letter{predicates=T}, State);
pronounce(#letter{predicates=[#predicate{action={verb,_},id=ID}=P|T]}=Letter, State) ->    
        case ID of
            <<>> ->
                say(P#predicate{id=fresh_id()}, State);
            _ ->
                say(P, State)
    end,
    pronounce(Letter#letter{predicates=T}, State);
pronounce(_, _) ->
    ok. % Empty Minded

-spec say(#predicate{}, #state{}) -> any().
say(#predicate{}=P, #state{herald=Herald, vassal=Vassal}=State) ->
    lager:info("Say: ~p~n", [P]),
    pronounce(Herald:excerpt(P), State#state{last_predicate=P}),
    Vassal:work(P).

-spec refrain(#predicate{}, #state{}) -> any().
refrain(#predicate{}=P, #state{scribe=Scribe}=_State) ->
    lager:info("Refrain: ~p ~n", [P]),
    Key = compose_key(P),
    Scribe:curb(Key, P).

-spec hear(#letter{}, #state{}) -> any().
hear(#letter{predicates=[#predicate{action={adverb,_}}=P|T]}=Letter, #state{scribe=Scribe}=State) ->
    lager:info("Hear: ~p ~n", [P]),
    case Scribe:recall(compose_key(P)) of
        undefined -> 
            E = Scribe:recall(compose_key(P#predicate{adjectives=[]}));
        E = #predicate{} ->
            ok
    end,
    Excerpt = Scribe:excerpt(E),
    pronounce(Excerpt, State),
    hear(Letter#letter{predicates=T}, State);
hear(_, _) ->    
    ok. % We don't take actions based on what we hear

-spec proclaim(#letter{}, #state{}) -> any().
proclaim(#letter{}=Letter, #state{scribe=Herald}) ->
    Herald:proclaim(Letter);
proclaim(_, _) ->    
    ok. % We don't take actions based on what we don't know

% Utils Functions
-spec fresh_id() -> binary().
fresh_id() ->
    gen_server:call(clexical, fresh_id).

-spec compose_key(#predicate{}) -> binary().
compose_key(#predicate{adjectives={dict, _, _, _, _, _, _, _, _}=Dict}=P) ->
    BareKey = compose_key(P#predicate{adjectives=[]}),
    Suffix = dict:fold(fun(_K, V, A) -> <<A/binary, V/binary>> end, <<>>, Dict),
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