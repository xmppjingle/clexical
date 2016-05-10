-type kind() :: adverb | verb.

-record(predicate, {id = <<>> :: binary(), subject = <<>> :: binary(), action :: {kind(), binary()}, adjectives = undefined :: dict()|undefined, abstract = undefined :: any(), author = undefined :: any()}).
-record(letter, {predicates = [] :: [#predicate{}], sender :: any(), type :: decree|bulletin}).
