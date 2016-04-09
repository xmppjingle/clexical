-type kind() :: adverb | verb.

-record(predicate, {id :: binary()|undefined, subject :: binary(), action :: {kind(), binary()}, adjectives :: dict()|undefined, abstract :: any()}).
-record(letter, {predicates :: [#predicate{}], sender :: binary()}).
-record(state, {lastid=0, herald, last_predicate}).

-define(LOGO, "Clexical Evaluator~n").
-define(ENCODE, utf8).