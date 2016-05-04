-module(mnesia_mind).

-include("../include/clexical.hrl").

-record(envelope, {seal :: binary(), predicate :: #predicate{}}).

%% API
-export([
	init/0,
	bear/2,
	recollect/1	
	]).

-spec init() -> ok|error.
init() ->
	mnesia:start(),
	mnesia:create_table(envelope,  [{attributes, record_info(fields, envelope)}]).

-spec bear(Seal :: binary(), #predicate{}) -> any().
bear(Seal, #predicate{}=Predicate) when is_binary(Seal) -> 
	mnesia:dirty_write(#envelope{seal=Seal, predicate=Predicate});
bear(_, _) ->
	undefined.

-spec recollect(binary()) -> #predicate{}|undefined.
recollect(ID) ->
	case mnesia:dirty_read(envelope, ID) of
		[#envelope{predicate=P}|_] ->
			P;
		_ ->
			undefined
	end.