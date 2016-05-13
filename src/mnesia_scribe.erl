-module(mnesia_scribe).
-behaviour(scribe).

-include("../include/clexical.hrl").

-record(envelope, {seal :: binary(), predicate :: #predicate{}}).

%% API
-export([
	init/1,
	curb/2,
	recall/1
	]).

-spec init(Opts :: any()) -> ok|error.
init(_Opts) ->
	mnesia:start(),
	mnesia:create_table(envelope,  [{attributes, record_info(fields, envelope)}]).

-spec curb(Seal :: binary(), #predicate{}) -> any().
curb(Seal, #predicate{}=Predicate) when is_binary(Seal) -> 
	mnesia:dirty_write(#envelope{seal=Seal, predicate=Predicate});
curb(_, _) ->
	undefined.

-spec recall(binary()) -> #predicate{}|undefined.
recall(ID) ->
	case mnesia:dirty_read(envelope, ID) of
		[#envelope{predicate=P}|_] ->
			% mnesia:dirty_delete(envelope, ID),
			P;
		_ ->
			undefined
	end.