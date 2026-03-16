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
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:call(?MODULE, stop).

init(_) ->
    {ok, #state{id=0}}.

handle_info(_Record, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_call(fresh_id, _From, #state{id=LID}=State) ->
    ID = LID + 1,
    %% Use a combination of monotonic time and counter for uniqueness.
    Ts = erlang:monotonic_time(microsecond),
    BID = integer_to_binary(ID),
    BTs = integer_to_binary(Ts),
    {reply, <<BTs/binary, "-", BID/binary>>, State#state{id=ID}};

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Info, _From, State) ->
    {reply, ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ---------------------------------------------------------------------------
%% API
%% ---------------------------------------------------------------------------

-spec fresh_id() -> binary().
fresh_id() ->
    gen_server:call(?MODULE, fresh_id).
