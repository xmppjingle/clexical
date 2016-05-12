-module(scribe).
-include("../include/clexical.hrl").

-callback init(Opts::any()) -> any().

-callback curb(Key :: binary(), P :: #predicate{}) -> ok|error.
-callback recall(Key :: binary()) -> #predicate{}|undefined.

-callback letter_from_binary(Binary :: binary()) -> undefined|#letter{}.
-callback to_binary(#letter{}) -> undefined|binary().

-callback excerpt(#predicate{}) -> []|[#predicate{}].
