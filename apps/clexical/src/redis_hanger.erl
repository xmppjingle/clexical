-module(redis_hanger).
-behaviour(clexical_hanger).

%% API
-export([
	hang/2
	]).

hang(Key, Value) ->
	ok.