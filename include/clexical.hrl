-define(ENCODE, utf8).

-type kind() :: preposition | verb.

-record(predicate, {id = <<>> :: binary(), subject = <<>> :: binary(), action :: {kind(), binary()}, adjectives = undefined :: dict()|undefined, abstract = undefined :: any(), author = undefined :: any()}).
-record(letter, {predicates = [] :: [#predicate{}], author :: any(), type :: decree|bulletin}).
