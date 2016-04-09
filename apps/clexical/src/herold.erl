-module(herold).
-include("../include/clexical.hrl").

-callback curb(Key :: binary(), P :: #predicate{}) -> ok|error.
-callback recall(Key :: binary()) -> #predicate{}|undefined.
-callback read_excerpt(#predicate{}) -> #letter{}|undefined.
-callback proclaim(#predicate{}) -> ok|error.
