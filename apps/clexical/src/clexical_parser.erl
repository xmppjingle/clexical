-module(clexical_parser).
-include("../include/clexical.hrl").

-callback predicates_from_binary(Binary :: binary()) -> error|[#predicate{}].
-callback to_binary(#predicate{}) -> undefined|binary().
-callback kin_from_predicate(#predicate{}) -> []|[#predicate{}].
