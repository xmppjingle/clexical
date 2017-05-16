-define(ENCODE, utf8).

-type kind() :: preposition | verb.

-record(predicate, {
	id = <<>> :: binary(),
	subject = <<>> :: binary(),
	author = undefined :: any(),
	action :: {kind(), binary()}, 
	adjectives = undefined :: map()|undefined, 
	abstract = undefined :: any()
	}).

-record(letter, {
	subject :: any() | <<>>,
	author :: any() | <<>>, 
	recipient :: any() | <<>>,
	predicates = [] :: [#predicate{}], 
	via :: any() | undefined,
	envelop :: any() | <<>>,
	original :: any() | <<>>,
	type :: decree|bulletin
	}).

-define(ANY_ID, <<"*ID*">>).
-define(ANY_SUBJECT, <<"*SUBJECT*">>).
-define(ANY_RECIPIENT, <<"*RECIPIENT*">>).
-define(ANY_AUTHOR, <<"*AUTHOR*">>).