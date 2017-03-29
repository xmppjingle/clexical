-module(linguist).
-include("../include/clexical.hrl").

-callback get_envelop_type(Envelop::binary()) -> decree|bulletin|undefined.

-callback get_sentence_type(Sentence::binary()) -> preposition|verb|undefined.