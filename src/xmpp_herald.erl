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
	predicate_from_elem/2,
	validate/1
	]).

-export([
	letter_from_xmlel/1,
	get_attr/2,
	get_attr/3
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

handle_info(_Info, S) ->
    lager:debug("Info: ~p  ~n", [_Info]),
    {noreply, S}.

get_linguist() ->
	?MODULE ! {self(), linguist},
	receive
		{linguist, Linguist} ->
			Linguist;
		_ ->
			?MODULE
		after
			1000 ->
				lager:error("Failed to retrieve Linguist on: ~p  ~n", [?MODULE]),
				?MODULE
	end.

initialize(Opts) ->
	lager:debug("Initialize with ~p ~n", [Opts]),
	start_link(Opts).

-spec proclaim(#letter{}) -> ok|error.
proclaim(#letter{author=Author}=L) -> 
	lager:info("Proclamation: ~p -> ~p~n", [L, Author]),
	case erlang:is_pid(Author) of
		true ->
			Bin = to_binary(L),
			Author ! {send, Bin};
		_ ->
			lager:debug("No Destination: ~p ~n", [L]),
			ok
	end.

-spec letter_from_binary(Binary :: binary()) -> undefined|#letter{}.
letter_from_binary(Bin) ->
	letter_from_xmlel( fxml_stream:parse_element(Bin) ).

-spec letter_from_xmlel(#xmlel{}) -> undefined|#letter{}.
letter_from_xmlel(#xmlel{name = Type, children = Children, attrs = Attrs}) ->
	Linguist = get_linguist(),
	Author = get_attr(<<"from">>, Attrs, ?ANY_SUBJECT),
	Recipient = get_attr(<<"to">>, Attrs, ?ANY_SUBJECT),
	P = [Predicate || Predicate <- lists:map(fun(E) -> Linguist:predicate_from_elem(E, Author) end, Children), Predicate /= undefined],
	#letter{type = Linguist:get_envelop_type(Type), predicates=P, author=Author, recipient = Recipient};
letter_from_xmlel(_R) -> 
	lager:info("Invalid Letter Type: ~p ~n", [_R]),
	undefined.

-spec to_binary(#letter{}) -> undefined|binary().
to_binary(#letter{predicates = [], type = Type, author = Author, recipient = Recipient}) ->
	BType = get_envelop(Type),
	<<"<",  BType/binary, " from='", Author/binary, "' to='", Recipient/binary, "'/>">>;
to_binary(#letter{predicates = PS, type = Type, author = Author, recipient = Recipient}) ->
	PP = to_binary_(PS),
	BType = get_envelop(Type),
	<<"<", BType/binary, " from='", Author/binary, "' to='", Recipient/binary, "'>", PP/binary, "</", BType/binary, ">">>;
to_binary(_) ->
	<<>>.

to_binary_([]) ->
	<<>>;
to_binary_([#predicate{subject = Subject, abstract = #xmlel{attrs = Attribs} = Elem}|T])->
	P = fxml:element_to_binary(Elem#xmlel{attrs = [ {<<"subject">>, Subject} | Attribs]}),
	PP = to_binary_(T),
	<<P/binary, PP/binary>>.

-spec excerpts(#predicate{}) -> []|[#predicate{}].
excerpts(#predicate{abstract=undefined}) ->
	[];
excerpts(#predicate{abstract=#xmlel{children = Kin}, author=Author}) ->
	Linguist = get_linguist(),
	lists:map(fun(Elem) -> Linguist:predicate_from_elem(Elem, Author) end, Kin);
excerpts(_) ->
	[].

predicate_from_elem(#xmlel{name = ActionName, attrs = Attribs} = E, Author) ->
	ID = get_attr(<<"id">>, Attribs, ?ANY_ID),
	Subject = get_attr(<<"subject">>, Attribs, ?ANY_SUBJECT),
	Adjectives = maps:from_list(Attribs),
	Linguist = get_linguist(),
	Linguist:validate(#predicate{id=ID, subject=Subject, action={Linguist:get_sentence_type(ActionName), ActionName}, adjectives=Adjectives, abstract=E, author = Author});
predicate_from_elem({xmlcdata, <<"\n">>}, _) -> undefined;
predicate_from_elem({xmlcdata, Data}, _) ->
	lager:info("Received Data: ~p ~n", [Data]),
	undefined;
predicate_from_elem(_, _) -> undefined.

validate(#predicate{id = ID, subject = Subject} = P) when ID /= undefined, Subject /= undefined ->
	lager:debug("Valid Predicate: ~p ~n", [P]),
	P;
validate(P) -> 
	lager:debug("Discarding invalid Predicate: ~p ~n", [P]),
	undefined.

get_envelop_type(<<"iq">>) ->
	decree;
get_envelop_type(<<"presence">>) ->
	bulletin.

get_envelop(decree) ->
	<<"iq">>;
get_envelop(bulletin) ->
	<<"presence">>.

get_sentence_type(<<"on",_/binary>>) ->
	preposition;
get_sentence_type(_) ->
    verb.

get_attr(ID, Attribs) ->
	get_attr(ID, Attribs, undefined).

get_attr(ID, Attribs, Default) ->
	case fxml:get_attr(ID, Attribs) of
		{value, Value} -> 
			Value;
		_ ->
			Default
	end.

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