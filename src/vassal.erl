-module(vassal).
-include("../include/clexical.hrl").

-callback init(Args::any()) -> any().
-callback work(#predicate{}) -> ok|error.
