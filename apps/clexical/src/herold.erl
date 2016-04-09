-module(herold).
-include("../include/clexical.hrl").

-callback curb(Key :: binary(), P :: #predicate{}) -> ok|error.
-callback read_excerpt(#predicate{}) -> #letter{}.
-callback proclaim(#predicate{}) -> ok|error.

