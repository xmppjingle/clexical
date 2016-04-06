-module(clexical).
-behaviour(gen_server).

-include("../include/clexical.hrl").

%% gen_server callbacks
-export([
    start_link/2,
    stop/0,
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

%% API Methods
-export([
]).

start_link(_, _) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:call(?MODULE, stop).

init(_Params) ->
    lager:info(?LOGO,[]),
    {ok, #state{}}.

handle_info(Record, State) ->
    lager:debug("Unknown Info Request: ~p~n", [Record]),
    {noreply, State}.

handle_cast(_Msg, State) ->
    lager:debug("Received Cast: ~p~n", [_Msg]),
    {noreply, State}.

handle_call(Info, _From, _State) ->
    lager:info("Received Call: ~p~n", [Info]),
    {reply, ok, _State}.

terminate(_Reason, _State) ->
    lager:info("Terminated Component.", []),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
