-module(mnesia_scribe).
-behaviour(scribe).

-include("../include/clexical.hrl").

-record(envelope, {seal :: binary(), predicate :: #predicate{}}).

%% API
-export([
	initialize/1,
	curb/2,
	recall/1
	]).

-export([
	clear/0
	]).

-spec initialize(Opts :: any()) -> ok|error.
initialize(_Opts) ->
	mnesia:start(),
	mnesia:create_table(envelope,  [{attributes, record_info(fields, envelope)}]),
	lager:info("MNesia Scribe Started with: ~p~n", [_Opts]).

-spec curb(Seal :: binary(), #predicate{}) -> any().
curb(Seal, #predicate{}=Predicate) when is_binary(Seal) -> 
	lager:debug("Curb[~p]: ~p ~n", [Seal, Predicate]),
	mnesia:dirty_write(#envelope{seal=Seal, predicate=Predicate});
curb(_K, _P) ->
	lager:error("Invalid Seal[~p] or Predicate: ~p", [_K, _P]),
	undefined.

-spec recall(binary()) -> #predicate{}|undefined.
recall(ID) ->
	lager:debug("Try Recall[~p]... ~n", [ID]),
	case mnesia:dirty_read(envelope, ID) of
		[#envelope{predicate=P}|_] ->
			% mnesia:dirty_delete(envelope, ID),
			lager:debug("Recalled[~p] of ~p~n", [ID, P]),
			P;
		_ ->
			undefined
	end.

clear() ->
	case mnesia:clear_table(envelope) of
		{_, ok} ->
			ok;
		_ ->
			error
	end.