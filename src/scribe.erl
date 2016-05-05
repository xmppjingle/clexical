-module(scribe).
-include("../include/clexical.hrl").

-callback curb(Key :: binary(), P :: #predicate{}) -> ok|error.
-callback recall(Key :: binary()) -> #predicate{}|undefined.
-callback excerpt(#predicate{}) -> #letter{}|undefined.

