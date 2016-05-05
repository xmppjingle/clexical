-module(herald).
-include("../include/clexical.hrl").

-callback init(Opts::any()) -> any().
-callback proclaim(#letter{}) -> ok|error.
