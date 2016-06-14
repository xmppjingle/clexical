-module(herald).
-include("../include/clexical.hrl").

-callback init(Opts::any()) -> any().

-callback proclaim(#letter{}) -> ok|error.

-callback letter_from_binary(Binary :: binary()) -> undefined|#letter{}.

-callback to_binary(#letter{}) -> undefined|binary().

-callback excerpts(#predicate{}) -> []|[#predicate{}].
