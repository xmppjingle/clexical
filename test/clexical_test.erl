%%
%% clexical_test — Stub herald/scribe/vassal implementation for unit tests.
%%
%% Used by tests that need a lightweight in-process implementation of
%% the three behaviours without HTTP, Mnesia, or XMPP.
%%
-module(clexical_test).
-behaviour(herald).
-behaviour(scribe).
-behaviour(vassal).

-include("../include/clexical_test.hrl").

-export([
    initialize/1,
    proclaim/1,
    excerpts/1,
    letter_from_binary/1,
    to_binary/1,
    curb/2,
    recall/1,
    clear/0,
    work/2
]).

initialize(_Opts) -> ok.

proclaim(_Letter) -> ok.

excerpts(#predicate{abstract = E}) -> E.

letter_from_binary(_) -> undefined.

to_binary(_) -> <<>>.

curb(_K, _V) -> ok.

recall(_K) -> undefined.

clear() -> ok.

work(#letter{} = L, _LP) -> L.
