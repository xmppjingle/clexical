-module(clexical).

-export([
	init/3,
	handle/2,
	terminate/3
	]).

init(_Transport, Req, []) ->
	{ok, Req, undefined}.

terminate(_Reason, _Req, _State) ->
	ok.

handle(Req, State) ->
	{Method, Req2} = cowboy_req:method(Req),
	{Dict, _}= cowboy_req:qs_vals(Req),
	{Filename, _} = cowboy_req:qs_val(<<"template">>, Req, <<"./default.xml">>),
	Template = bbmustache:parse_file(filename:join([filename:absname(""), filename:absname(filename:basename(Filename), "./templates/")])),
	Body = bbmustache:compile(Template, Dict, [{key_type, binary}]),
	{ok, Req4} = response(Method, Body, Req2),
	{ok, Req4, State}.

response(<<"GET">>, Body, Req) ->
	cowboy_req:reply(200,
		[{<<"content-encoding">>, <<"utf-8">>}, {<<"content-type">>, <<"application/xml">>}], Body, Req);
response(_, _, Req) ->
	%% Method not allowed.
	cowboy_req:reply(405, Req).
