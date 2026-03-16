%%
%% http_webhook_in_handler — Inbound webhook endpoint.
%%
%%   GET  /api/v1/webhooks/in/:name
%%   POST /api/v1/webhooks/in/:name
%%
%% No API-key authentication — these endpoints are designed to be called by
%% external systems that supply an optional HMAC-SHA256 signature.
%%
%% If the matching #webhook_reg{} has a secret, the request body MUST carry
%% a valid signature in the X-Webhook-Signature header:
%%   X-Webhook-Signature: sha256=<lowercase hex of HMAC-SHA256(secret, body)>
%%
%% The request is translated into a #letter{} via the endpoint's
%% letter_template (Mustache-rendered against query-params + body map), then
%% dispatched through clexical:attend/1 (stored + executed) by default or
%% clexical:recite/1 when the query param mode=recite is present.
%%
%% Query parameters are merged into the render context under "params".
%% For POST requests the decoded JSON body fields are merged at the top level
%% of the render context.
%%
%% Response:
%%   200 {"status": "dispatched", "subject": "..."}
%%   400 bad request / invalid JSON
%%   401 invalid or missing signature
%%   404 unknown webhook name
%%

-module(http_webhook_in_handler).

-include("../include/clexical.hrl").
-include("../include/clexical_webhooks.hrl").

-export([init/2]).

init(Req0, State) ->
    Name   = cowboy_req:binding(name, Req0),
    Method = cowboy_req:method(Req0),
    handle(Method, Name, Req0, State).

%% GET — body is empty; query params form the context
handle(<<"GET">>, Name, Req0, State) ->
    case lookup_endpoint(Name) of
        {error, not_found} ->
            reply(404, #{<<"error">> => <<"webhook not found">>}, Req0, State);
        {ok, Reg} ->
            case Reg#webhook_reg.enabled of
                false ->
                    reply(404, #{<<"error">> => <<"webhook not found">>}, Req0, State);
                true  ->
                    QsMap = qs_to_map(cowboy_req:parse_qs(Req0)),
                    Ctx   = #{<<"params">> => QsMap},
                    dispatch_webhook(Reg, <<>>, Ctx, Req0, State)
            end
    end;

%% POST — body may be JSON; merged into context
handle(<<"POST">>, Name, Req0, State) ->
    {ok, RawBody, Req1} = cowboy_req:read_body(Req0),
    case lookup_endpoint(Name) of
        {error, not_found} ->
            reply(404, #{<<"error">> => <<"webhook not found">>}, Req1, State);
        {ok, Reg} ->
            case Reg#webhook_reg.enabled of
                false ->
                    reply(404, #{<<"error">> => <<"webhook not found">>}, Req1, State);
                true  ->
                    case verify_signature(Reg, RawBody, Req1) of
                        {error, Req2} ->
                            {ok, Req2, State};
                        ok ->
                            QsMap    = qs_to_map(cowboy_req:parse_qs(Req1)),
                            BodyCtx  = decode_body_ctx(RawBody),
                            Ctx      = maps:merge(BodyCtx, #{<<"params">> => QsMap,
                                                             <<"raw_body">> => RawBody}),
                            dispatch_webhook(Reg, RawBody, Ctx, Req1, State)
                    end
            end
    end;

handle(_, _, Req0, State) ->
    reply(405, #{<<"error">> => <<"method_not_allowed">>}, Req0, State).

%% ---------------------------------------------------------------------------
%% Internal
%% ---------------------------------------------------------------------------

lookup_endpoint(Name) ->
    case webhook_vassal:get_endpoint(Name) of
        {ok, Map} ->
            %% Re-read the full record for access to the secret field
            case mnesia:dirty_read(webhook_reg, Name) of
                [R] -> {ok, R};
                []  ->
                    %% fall back to map-derived minimal record (no secret check)
                    {ok, #webhook_reg{
                        id              = Name,
                        letter_template = maps:get(<<"letter">>, Map, #{}),
                        enabled         = maps:get(<<"enabled">>, Map, true),
                        secret          = undefined,
                        created_at      = 0
                    }}
            end;
        {error, _} -> {error, not_found}
    end.

verify_signature(#webhook_reg{secret = undefined}, _Body, _Req) ->
    ok;
verify_signature(#webhook_reg{secret = Secret}, Body, Req) ->
    Expected = <<"sha256=", (hmac_hex(Secret, Body))/binary>>,
    Provided = cowboy_req:header(<<"x-webhook-signature">>, Req, <<>>),
    case constant_compare(Expected, Provided) of
        true  -> ok;
        false ->
            Req2 = cowboy_req:reply(401,
                #{<<"content-type">> => <<"application/json">>},
                jsone:encode(#{<<"error">> => <<"invalid signature">>}),
                Req),
            {error, Req2}
    end.

hmac_hex(Secret, Body) ->
    Mac = crypto:mac(hmac, sha256, Secret, Body),
    %% binary:encode_hex/2 with lowercase is OTP 26+; use lowercase/1 for compat.
    list_to_binary(string:lowercase(binary_to_list(binary:encode_hex(Mac)))).

constant_compare(A, B) when byte_size(A) =:= byte_size(B) ->
    crypto:hash(sha256, A) =:= crypto:hash(sha256, B);
constant_compare(_, _) ->
    false.

qs_to_map(QsList) ->
    lists:foldl(fun({K, V}, Acc) ->
        maps:put(K, V, Acc)
    end, #{}, QsList).

decode_body_ctx(<<>>) -> #{};
decode_body_ctx(Body) ->
    try jsone:decode(Body, [{object_format, map}])
    catch _:_ -> #{<<"_body">> => Body}
    end.

dispatch_webhook(#webhook_reg{letter_template = Template} = Reg, _RawBody, Ctx, Req0, State) ->
    Letter = render_letter(Template, Ctx),
    Mode   = case cowboy_req:match_qs([{mode, [], <<"attend">>}], Req0) of
                 #{mode := <<"recite">>} -> recite;
                 _                       -> attend
             end,
    Result = case Mode of
        attend -> clexical:attend(Letter);
        recite -> clexical:recite(Letter)
    end,
    logger:debug("http_webhook_in ~s dispatched ~p -> ~p", [Reg#webhook_reg.id, Mode, Result]),
    reply(200, #{<<"status">> => <<"dispatched">>, <<"subject">> => Letter#letter.subject}, Req0, State).

render_letter(Template, Ctx) when is_map(Template) ->
    %% Render each binary value in the template map as a Mustache template
    Rendered = maps:map(fun(_K, V) -> render_value(V, Ctx) end, Template),
    http_herald:json_to_letter(Rendered).

render_value(V, Ctx) when is_binary(V) ->
    try bbmustache:render(V, Ctx, [{key_type, binary}])
    catch _:_ -> V
    end;
render_value(V, _Ctx) -> V.

reply(Status, Body, Req0, State) ->
    Req = cowboy_req:reply(Status,
        #{<<"content-type">> => <<"application/json">>},
        jsone:encode(Body), Req0),
    {ok, Req, State}.
