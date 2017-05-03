-module(clexical_utils).

-include_lib("xmpp.hrl").

-export([
    remove_whitespaces_deeply/1
    ]).

is_whitespace({xmlcdata, CData}) ->
    is_whitespace2(CData);
is_whitespace(_) ->
    false.

is_whitespace2(<<C:8, Rest/binary>>)
  when C == $\s; C == $\t; C == $\n; C == $\r ->
    is_whitespace2(Rest);
is_whitespace2(<<>>) ->
    true;
is_whitespace2(_CData) ->
    false.

remove_whitespaces_deeply(#xmlel{children = Children} = XML_Element) ->
    New_Children = remove_whitespaces_deeply2(Children),
    XML_Element#xmlel{children = New_Children};
remove_whitespaces_deeply(List) ->
    [ remove_whitespaces_deeply(E) || E <- List].

remove_whitespaces_deeply2(undefined) ->
    undefined;
remove_whitespaces_deeply2(Children) ->
    remove_whitespaces_deeply3(Children, []).

remove_whitespaces_deeply3([El | Rest], Result)
  when is_record(El, xmlel)->
    New_El = remove_whitespaces_deeply(El),
    remove_whitespaces_deeply3(Rest, [New_El | Result]);
remove_whitespaces_deeply3([{xmlcdata, CData} | Rest], Result) ->
    case is_whitespace(CData) of
        true  -> remove_whitespaces_deeply3(Rest, Result);
        false -> remove_whitespaces_deeply3(Rest, [{xmlcdata, CData} | Result])
    end;
remove_whitespaces_deeply3([Other | Rest], Result) ->
    remove_whitespaces_deeply3(Rest, [Other | Result]);
remove_whitespaces_deeply3([], Result) ->
    lists:reverse(Result).