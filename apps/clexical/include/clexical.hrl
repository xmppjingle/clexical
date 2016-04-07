-type kind() :: adverb | verb.

-record(predicate, {id :: binary(), subject :: binary(), action :: {kind(), atom()}, adjectives :: dict(), content :: [#predicate{}]}).
-record(state, {lastid=0, parser, runner, hanger}).

-define(LOGO, "Clexical Evaluator~n").
