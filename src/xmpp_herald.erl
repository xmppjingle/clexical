-module(xmpp_herald).
-include("../include/clexical.hrl").
-include_lib("xmpp.hrl").
-behaviour(gen_server).
-behaviour(herald).
-behaviour(xmpp_linguist).

%% gen_server callbacks
-export([
	init/1,
    start_link/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-export([
	initialize/1,
    proclaim/1,
	excerpts/1,
	letter_from_binary/1,
	to_binary/1,
	process_letter/1
	]).

-export([
	get_envelop_type/1,
	get_sentence_type/1,
	predicate_from_elem/3,
	validate/1
	]).

-export([
	letter_from_xmlel/1,
	get_attr/2,
	get_attr/3,
	camel/1,
	merge_adjectives/1,
	merge_adjectives/2
	]).

-define(OK, <<"<ok/>">>).
-define(ERROR, <<"<error/>">>).

-record(hdata, {linguist = ?MODULE}).

start_link(Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Opts, []).

-spec init(Opts::any()) -> any().
init(Opts) ->
	Linguist = proplists:get_value(linguist, Opts, ?MODULE),
	lager:info("XMPP Herald Started with: ~p~n", [Opts]),
	{ok, #hdata{linguist = Linguist}}.

terminate(_, _) ->
    lager:debug("Terminating...~n~p", [?MODULE]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_call({linguist}, _From, #hdata{linguist = Linguist} = S) ->
    {reply, Linguist, S};
handle_call(_Call, _From, S) ->
    lager:debug("Call: ~p  ~n", [_Call]),
    {reply, ok, S}.

handle_cast(_Cast, S) ->
    lager:debug("Cast: ~p  ~n", [_Cast]),
    {noreply, S}.

handle_info({Pid, linguist} = Cast, #hdata{linguist = Linguist} = S) ->
    lager:debug("Info: ~p  ~n", [Cast]),
    Pid ! {linguist, Linguist},
    {noreply, S};

handle_info({proclaim, #letter{via = Via} = L}, #hdata{linguist = ?MODULE} = S) ->
	lager:debug("No Proclamation: ~p -> ~p~n", [L, Via]),
	{noreply, S};
handle_info({proclaim, #letter{} = L}, #hdata{linguist = Linguist} = S) ->
	spawn(Linguist, proclaim, [L]),
	{noreply, S};

handle_info(_Info, S) ->
    lager:debug("Info: ~p  ~n", [_Info]),
    {noreply, S}.

get_linguist() ->
	gen_server:call(?MODULE, {linguist}).

initialize(Opts) ->
	lager:debug("Initialize with ~p ~n", [Opts]),
	start_link(Opts).

-spec proclaim(#letter{}) -> ok|error.
proclaim(L) -> 
	?MODULE ! {proclaim, L}.

-spec letter_from_binary(Binary :: binary()) -> undefined|#letter{}.
letter_from_binary(Bin) ->
	case letter_from_xmlel( clexical_utils:remove_whitespaces_deeply(fxml_stream:parse_element(Bin)) ) of
		#letter{} = L -> L#letter{original = Bin};
		_ -> undefined
	end.

-spec letter_from_xmlel(#xmlel{}) -> undefined|#letter{}.
letter_from_xmlel(#xmlel{children = Children, attrs = Attrs} = XML) ->
	Linguist = get_linguist(),
	Author = get_attr(<<"from">>, Attrs, ?ANY_AUTHOR),
	Recipient = get_attr(<<"to">>, Attrs, ?ANY_RECIPIENT),
	Subject = get_attr(<<"id">>, Attrs, ?ANY_SUBJECT),
	Envelop = XML#xmlel{children = []},
	P = [clexical:fill_subject(Predicate, Subject) || Predicate <- lists:map(fun(E) -> Linguist:predicate_from_elem(E, Author, Envelop) end, Children), Predicate /= undefined],
	#letter{type = Linguist:get_envelop_type(XML), predicates = P, author = Author, recipient = Recipient, subject = Subject, envelop = Envelop, original = XML};
letter_from_xmlel(_R) -> 
	lager:info("Invalid Letter Type: ~p ~n", [_R]),
	undefined.

-spec to_binary(#letter{}) -> undefined|binary().
to_binary(#letter{predicates = [], type = decree, author = Author, recipient = Recipient, subject = ID}) ->
	<<"<iq from='", Author/binary, "' to='", Recipient/binary, "' id='", ID/binary, "' type='result'/>">>;
to_binary(#letter{predicates = PS, type = decree, author = Author, recipient = Recipient, subject = ID}) ->
	PP = to_binary_(PS),
	<<"<iq from='", Author/binary, "' to='", Recipient/binary, "' id='", ID/binary, "' type='set'>", PP/binary, "</iq>">>;

to_binary(#letter{predicates = [], type = bulletin, author = Author, recipient = Recipient, subject = ID, envelop = #xmlel{name = <<"iq">>}}) ->
	<<"<iq from='", Author/binary, "' to='", Recipient/binary, "' id='", ID/binary, "'/>">>;
to_binary(#letter{predicates = PS, type = bulletin, author = Author, recipient = Recipient, subject = ID, envelop = #xmlel{name = <<"iq">>}}) ->
	PP = to_binary_(PS),
	<<"<iq from='", Author/binary, "' to='", Recipient/binary, "' id='", ID/binary, "' type='result'>", PP/binary, "</iq>">>;

to_binary(#letter{predicates = [], type = bulletin, author = Author, recipient = Recipient, subject = ID}) ->
	<<"<presence from='", Author/binary, "' to='", Recipient/binary, "' id='", ID/binary, "'/>">>;
to_binary(#letter{predicates = PS, type = bulletin, author = Author, recipient = Recipient, subject = ID}) ->
	PP = to_binary_(PS),
	<<"<presence from='", Author/binary, "' to='", Recipient/binary, "' id='", ID/binary, "'>", PP/binary, "</presence>">>;
to_binary(_) ->
	<<>>.

to_binary_([]) ->
	<<>>;
to_binary_([#predicate{subject = Subject, id = ID, abstract = #xmlel{attrs = Attribs} = Elem}|T])->
	P = fxml:element_to_binary(Elem#xmlel{attrs = [ {<<"subject">>, Subject}, {<<"id">>, ID} | Attribs]}),
	PP = to_binary_(T),
	<<P/binary, PP/binary>>.

-spec excerpts(#predicate{}) -> []|[#predicate{}].
excerpts(#predicate{abstract=undefined}) ->
	[];
excerpts(#predicate{abstract=#xmlel{children = Kin} = XML, author=Author, subject = Subject}) ->
	Linguist = get_linguist(),
	Envelop = XML#xmlel{children = []},
	[ clexical:fill_subject(Predicate, Subject) || Predicate <- lists:map(fun(E) -> Linguist:predicate_from_elem(E, Author, Envelop) end, Kin), Predicate /= undefined];
excerpts(#predicate{abstract = BinXML, author=Author, subject = Subject}) when is_binary(BinXML) ->
	Linguist = get_linguist(),
	#xmlel{children = Kin} = XML = clexical_utils:remove_whitespaces_deeply(fxml_stream:parse_element(BinXML)),
	Envelop = XML#xmlel{children = []},
	[ clexical:fill_subject(Predicate, Subject) || Predicate <- lists:map(fun(E) -> Linguist:predicate_from_elem(E, Author, Envelop) end, Kin), Predicate /= undefined];
excerpts(_) ->
	[].

predicate_from_elem(#xmlel{name = ActionName, attrs = Attribs} = E, Author, Envelop) ->
	ID = get_attr(<<"id">>, Attribs, get_attr(<<"id">>, Envelop, ?ANY_ID)),
	Subject = get_attr(<<"subject">>, Attribs, ?ANY_SUBJECT),
	Adjectives = maps:from_list(Attribs),
	Linguist = get_linguist(),
	Linguist:validate(#predicate{id = ID, subject = Subject, action = {Linguist:get_sentence_type(ActionName), ActionName}, adjectives = Adjectives, abstract = E, author = Author});
predicate_from_elem({xmlcdata, <<"\n">>}, _, _) -> undefined;
predicate_from_elem({xmlcdata, Data}, _, _) ->
	lager:debug("Received Data: ~p ~n", [Data]),
	undefined;
predicate_from_elem(_, _, _) -> undefined.

validate(#predicate{id = ID, subject = Subject} = P) when ID /= undefined, Subject /= undefined ->
	P;
validate(P) -> 
	lager:debug("Discarding invalid Predicate: ~p ~n", [P]),
	undefined.

get_envelop_type(#xmlel{name = <<"iq">>}) ->
	decree;
get_envelop_type(#xmlel{name = <<"presence">>}) ->
	bulletin;
get_envelop_type(_) ->
	bulletin.

get_sentence_type(<<"on",_/binary>>) ->
	preposition;
get_sentence_type(_) ->
    verb.

camel(<<>>) -> <<>>;
camel(<<C:8, Tail/binary>>) -> <<(C-32), Tail/binary>>.

get_attr(ID, Attribs) ->
	get_attr(ID, Attribs, undefined).

get_attr(ID, #xmlel{attrs = Attribs}, Default) ->
	case fxml:get_attr(ID, Attribs) of
		{value, Value} -> 
			Value;
		_ ->
			Default
	end;
get_attr(ID, Attribs, Default) when is_list(Attribs) ->
	case fxml:get_attr(ID, Attribs) of
		{value, Value} -> 
			Value;
		_ ->
			Default
	end;
get_attr(_ID, _, Default) ->
	Default.

merge_adjectives(#predicate{adjectives = Adjs} = P) ->
	merge_adjectives_(excerpts(P), Adjs);
merge_adjectives(_) -> #{}.

merge_adjectives_([#predicate{adjectives = Adjs} = P| R], MergedAdjs) ->
	M = merge_adjectives_(excerpts(P), MergedAdjs),
	merge_adjectives_(R, maps:merge(M, Adjs));
merge_adjectives_([], Adjs) ->
	Adjs.

merge_adjectives(#predicate{} = P1, #predicate{} = P2) ->
	maps:merge(merge_adjectives(P1), merge_adjectives(P2));
merge_adjectives(#predicate{} = P1, _) ->
	merge_adjectives(P1);
merge_adjectives(_, #predicate{} = P2) ->
	merge_adjectives(P2);
merge_adjectives(_, _) ->
	#{}.

process_letter(Letter) ->
	case Letter of
		#letter{type = decree} -> 
			clexical:recite(Letter),
			?OK;
		#letter{type = bulletin} -> 
			clexical:attend(Letter),
			?OK;
		_ ->
			?ERROR
	end.