-module(xmpp_linguist).
-include("../include/clexical.hrl").
-include_lib("fast_xml/include/fxml.hrl").

-callback get_envelop_type(Envelop::#xmlel{}) -> decree|bulletin|undefined.

-callback get_sentence_type(Sentence::binary()) -> preposition|verb|undefined.

-callback predicate_from_elem(Elem::#xmlel{}, Author::binary(), Envelop::#xmlel{}|undefined) -> #predicate{}|undefined.

-callback validate(#predicate{}) -> #predicate{}|undefined.

-callback proclaim(#letter{}) -> ok|error.