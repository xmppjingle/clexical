-module(vassal).
-include("../include/clexical.hrl").

-callback initialize(Opts::any()) -> any().
-callback work(#letter{}, #predicate{}) -> #letter{}|undefined.
