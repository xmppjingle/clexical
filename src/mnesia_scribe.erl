-module(mnesia_scribe).
-behaviour(scribe).

-include("../include/clexical.hrl").

-record(envelope, {seal :: binary(), predicate :: #predicate{}}).

%% API
-export([
    initialize/1,
    curb/2,
    recall/1,
    clear/0
]).

-spec initialize(Opts :: any()) -> ok | error.
initialize(_Opts) ->
    mnesia:start(),
    Dir = os:getenv("CLEXICAL_MNESIA_DIR", "/tmp/clexical_mnesia"),
    application:set_env(mnesia, dir, Dir),
    mnesia:create_table(envelope, [{attributes, record_info(fields, envelope)},
                                   {disc_copies, [node()]}]),
    logger:info("mnesia_scribe started, dir=~p", [Dir]),
    ok.

-spec curb(Seal :: binary(), #predicate{}) -> any().
curb(Seal, #predicate{}=Predicate) when is_binary(Seal) ->
    logger:debug("curb[~p]: ~p", [Seal, Predicate]),
    mnesia:dirty_write(#envelope{seal=Seal, predicate=Predicate});
curb(K, P) ->
    logger:error("mnesia_scribe: invalid seal ~p or predicate ~p", [K, P]),
    undefined.

-spec recall(binary()) -> #predicate{} | undefined.
recall(ID) ->
    logger:debug("recall[~p]...", [ID]),
    case mnesia:dirty_read(envelope, ID) of
        [#envelope{predicate=P}|_] ->
            logger:debug("recalled[~p]: ~p", [ID, P]),
            P;
        _ ->
            undefined
    end.

clear() ->
    case mnesia:clear_table(envelope) of
        {atomic, ok} -> ok;
        _            -> error
    end.
