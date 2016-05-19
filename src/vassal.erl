-module(vassal).
-include("../include/clexical.hrl").

-callback init(Opts::any()) -> any().
-callback work(#letter{}, #predicate{}) -> #letter{}|undefined.
