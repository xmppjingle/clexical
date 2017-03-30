-module(xmpp_linguist).
-include("../include/clexical.hrl").
-include_lib("xmpp.hrl").

-callback get_envelop_type(Envelop::binary()) -> decree|bulletin|undefined.

-callback get_sentence_type(Sentence::binary()) -> preposition|verb|undefined.

-callback predicate_from_elem(Elem::#xmlel{}, Author::binary()) -> #predicate{}|undefined.