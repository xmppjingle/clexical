-module(confetti).

-export([fetch/1, terminate/2]).

-spec fetch(atom()) -> list().
fetch(_Key) ->
    [].

-spec terminate(any(), any()) -> ok.
terminate(_Reason, _State) ->
    ok.
