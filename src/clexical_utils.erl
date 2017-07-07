-module(clexical_utils).

-include_lib("xmpp.hrl").

-export([
    remove_whitespaces_deeply/1,
    is_whitespace/1,
    get_document/1,
    read_whole_file/1,
    list_files_from_dir/1,
    proclaim_letters_from_dir/2,
    proclaim_letters_from_dir/3,
    proclaim_letters_from_dir/4,
    proclaim_template_for_values/3,
    render_process/4
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

remove_whitespaces_deeply3([#xmlel{} = El | Rest], Result) ->
    New_El = remove_whitespaces_deeply(El),
    remove_whitespaces_deeply3(Rest, [New_El | Result]);
remove_whitespaces_deeply3([{xmlcdata, _} = CData | Rest], Result) ->
    case is_whitespace(CData) of
        true  -> remove_whitespaces_deeply3(Rest, Result);
        false -> remove_whitespaces_deeply3(Rest, [CData | Result])
    end;
remove_whitespaces_deeply3([Other | Rest], Result) ->
    remove_whitespaces_deeply3(Rest, [Other | Result]);
remove_whitespaces_deeply3([], Result) ->
    lists:reverse(Result).

list_files_from_dir(Dir) ->
    case file:list_dir(Dir) of
        {ok, List} -> lists:filter(fun([H|_T]) -> [H] /= "." end, List);
        _ -> []
    end.

read_whole_file(FP) ->
    case file:read_line(FP) of
        {ok, [37|_LN]} ->
            read_whole_file(FP);
        {ok, LN} ->
            LN ++ read_whole_file(FP);
        _ ->
            file:close(FP),
            []
    end.

get_document(Filename) ->
    case file:open(Filename, [read]) of
        {ok, FP} ->
            read_whole_file(FP);
        {error,_} ->
            'invalid_filename'
    end.

proclaim_letters_from_dir(Dir, Herald) ->
    proclaim_letters_from_dir(Dir, Herald, #{}).
proclaim_letters_from_dir(Dir, Herald, Adjs) ->
    proclaim_letters_from_dir(Dir, Herald, Adjs, binary).    
proclaim_letters_from_dir(Dir, Herald, Adjs, KeyType) ->
    Files = list_files_from_dir(Dir),
    lists:foreach(fun(F) -> {ok, Bin} = file:read_file(Dir++"/"++F), render_process(Herald, Bin, Adjs, KeyType) end, Files).

render_process(Herald, Bin, Adjs, KeyType) ->
    Herald:process_letter(Herald:letter_from_binary(bbmustache:render(Bin, Adjs, [{key_type, KeyType}]))).

proclaim_template_for_values(Herald, Filename, DictFilename) ->
    case file:open(DictFilename, [read]) of
        {ok, FP} ->
            Template = get_document(Filename),
            proclaim_template_for_values_(Herald, Template, FP);
        {error,_} ->
            'invalid_filename'
    end.

proclaim_template_for_values_(Herald, Template, FP) ->
    case clexical_utils:map_from_file_line(FP) of
        eof ->
            ok;
        Map -> 
            render_process(Herald, Template, Map, binary),
            proclaim_template_for_values_(Herald, Template, FP)
    end.
