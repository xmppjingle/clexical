-module(herald).
-include("../include/clexical.hrl").

-callback proclaim(#letter{}) -> ok|error.
