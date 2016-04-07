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
    process/2    
]).

start_link(Parser, Runner, Hanger) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Parser, Runner, Hanger], []).

stop() ->
    gen_server:call(?MODULE, stop).

init([Parser, Runner, Hanger]) ->
    lager:info(?LOGO,[]),
    {ok, #state{parser=Parser, runner=Runner, hanger=Hanger}}.

handle_info(Record, State) ->
    lager:debug("Unknown Info Request: ~p~n", [Record]),
    {noreply, State}.

handle_cast(#predicate{}=P, State) ->
    lager:debug("Cast Predicate: ~p~n", [P]),
    spawn(?MODULE, process, [[P], State]),
    {noreply, State};
handle_cast(_Msg, State) ->
    lager:debug("Received Cast: ~p~n", [_Msg]),
    {noreply, State}.

handle_call(fresh_id, _From, #state{lastid=LID}=State) ->
    ID = LID + 1,
    {reply, erlang:integer_to_binary(ID), State#state{lastid=ID}};
handle_call({submit, Script}, _From, #state{parser=Parser}=State) when is_binary(Script) ->
    Result = case Parser:from_binary(Script) of
        [#predicate{}|_]=Predicates ->
            gen_server:abcast(?MODULE, Predicates),
            ok;
        _ ->
            error
    end,
    {reply, Result, State};
handle_call(Info, _From, _State) ->
    lager:info("Received Call: ~p~n", [Info]),
    {reply, ok, _State}.

terminate(_Reason, _State) ->
    lager:info("Terminated Component.", []),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Clexical Functions

process([#predicate{action={adverb,_}}=P|T],  #state{last_predicate=#predicate{id=ID, subject=Subject}}=State) ->
    pend(P#predicate{id=ID, subject=Subject}, State),
    process(T, State);
process([#predicate{action={verb,_}}=P|T], State) ->    
    execute(P#predicate{id=fresh_id()}, State),
    process(T, State);
process(_, _) ->
    ok.

execute(#predicate{content=Content, action=Action}=P, #state{runner=Runner}=State) ->    
    process(Content, State#state{last_predicate=P}),
    lager:debug("Executing [~p] ~n", [Action]),
    Runner:run(P).

pend(#predicate{action=Action}=P, #state{hanger=Hanger, parser=Parser}=_State) ->
    lager:debug("Pending [~p] ~n", [Action]),
    Key = compose_key(P),
    Value = Parser:to_binary(P),
    Hanger:hang(Key, Value).

compose_key(#predicate{adjectives={dict, _, _, _, _, _, _, _, _}=Dict}=P) ->
    BareKey = compose_key(P#predicate{adjectives=[]}),
    Suffix = dict:fold(fun(_K, V, A) -> A ++ V end, <<>>, Dict),
    <<BareKey/binary, Suffix/binary>>;
compose_key(#predicate{action={_,Name}, subject=Subject, id=ID}) ->
    <<Subject/binary, ID/binary, Name/binary>>;
compose_key(_) ->
    <<>>.    

fresh_id() ->
    gen_server:call(clexical, fresh_id).