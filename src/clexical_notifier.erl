%%
%% clexical_notifier — lightweight pub-sub for proclaim results.
%%
%% Processes can subscribe to receive #letter{} notifications.
%% The http_herald calls notify/1 whenever it proclaims a letter.
%% This allows websocket handlers, test processes, etc. to receive results.
%%

-module(clexical_notifier).
-behaviour(gen_server).

-include("../include/clexical.hrl").

-export([start_link/0, subscribe/0, subscribe/1, unsubscribe/0, notify/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
    subs = #{} :: #{pid() => reference()}
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Subscribe the calling process. Returns ok.
-spec subscribe() -> ok.
subscribe() ->
    subscribe(self()).

-spec subscribe(pid()) -> ok.
subscribe(Pid) ->
    gen_server:cast(?MODULE, {subscribe, Pid}).

-spec unsubscribe() -> ok.
unsubscribe() ->
    gen_server:cast(?MODULE, {unsubscribe, self()}).

-spec notify(#letter{}) -> ok.
notify(Letter) ->
    gen_server:cast(?MODULE, {notify, Letter}).

%% ---------------------------------------------------------------------------
%% gen_server
%% ---------------------------------------------------------------------------

init(_) ->
    {ok, #state{}}.

handle_cast({subscribe, Pid}, #state{subs = Subs} = State) ->
    Ref = erlang:monitor(process, Pid),
    {noreply, State#state{subs = maps:put(Pid, Ref, Subs)}};

handle_cast({unsubscribe, Pid}, #state{subs = Subs} = State) ->
    case maps:find(Pid, Subs) of
        {ok, Ref} ->
            erlang:demonitor(Ref, [flush]),
            {noreply, State#state{subs = maps:remove(Pid, Subs)}};
        error ->
            {noreply, State}
    end;

handle_cast({notify, Letter}, #state{subs = Subs} = State) ->
    maps:foreach(fun(Pid, _Ref) -> Pid ! {clexical_letter, Letter} end, Subs),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_call(_Req, _From, State) ->
    {reply, ok, State}.

handle_info({'DOWN', _Ref, process, Pid, _Reason}, #state{subs = Subs} = State) ->
    {noreply, State#state{subs = maps:remove(Pid, Subs)}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
