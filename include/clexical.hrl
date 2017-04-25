-define(ENCODE, utf8).

-type kind() :: preposition | verb.

-record(predicate, {
	id = <<>> :: binary(),
	subject = <<>> :: binary(), 
	action :: {kind(), binary()}, 
	adjectives = undefined :: map()|undefined, 
	abstract = undefined :: any(), 
	author = undefined :: any()
	}).

-record(letter, {
	predicates = [] :: [#predicate{}], 
	author :: any(), 
	recipient :: any(),
	via :: any() | undefined,
	type :: decree|bulletin
	}).

-define(ANY_ID, <<"*ID*">>).
-define(ANY_SUBJECT, <<"*SUBJECT*">>).