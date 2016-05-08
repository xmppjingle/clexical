-type kind() :: adverb | verb.

-record(predicate, {id = <<>> :: binary(), subject = <<>> :: binary(), action :: {kind(), binary()}, adjectives = undefined :: dict()|undefined, abstract = undefined :: any()}).
-record(letter, {predicates :: [#predicate{}], sender :: binary(), type :: decree|bulletin}).
