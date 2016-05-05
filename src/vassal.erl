-module(vassal).
-include("../include/clexical.hrl").

-callback work(#predicate{}) -> ok|error.
