-module(clexical_id).
-behaviour(gen_server).

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

-record(state, {id=0}).

%% API Functions
-export([
    fresh_id/0
]).

start_link(_Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [_Opts], []).

stop() ->
    gen_server:call(?MODULE, stop).

init(_) ->
    {ok, #state{id=0}}.

handle_info(Record, State) ->
    lager:debug("Unknown Info Request: ~p~n", [Record]),
    {noreply, State}.

handle_cast(_Msg, State) ->
    lager:debug("Received Cast: ~p~n", [_Msg]),
    {noreply, State}.

handle_call(fresh_id, _From, #state{id=LID}=State) ->
    ID = LID + 1,
    {{_, _, D}, _} = calendar:local_time(),
    BID = erlang:integer_to_binary(ID),
    BD = erlang:integer_to_binary(D),
    {reply, <<BD/binary, "@", BID/binary>>, State#state{id=ID}};

handle_call(Info, _From, _State) ->
    lager:info("Received Call: ~p~n", [Info]),
    {reply, ok, _State}.

terminate(_Reason, _State) ->
    lager:info("Terminated Component.", []),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% API

-spec fresh_id() -> binary().
fresh_id() ->
    gen_server:call(?MODULE, fresh_id).
