-module(clexical_parser).
-include("../include/clexical.hrl").

-callback letter_from_binary(Binary :: binary()) -> undefined|#letter{}.
-callback to_binary(#letter{}) -> undefined|binary().
-callback excerpt_from_predicate(#predicate{}) -> []|[#predicate{}].