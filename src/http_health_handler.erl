%%
%% http_health_handler — GET /health (no auth required).
%%

-module(http_health_handler).

-export([init/2]).

init(Req0, State) ->
    Body = jsone:encode(#{
        <<"status">> => <<"ok">>,
        <<"node">>   => atom_to_binary(node(), utf8),
        <<"vsn">>    => <<"0.2.0">>
    }),
    Req = cowboy_req:reply(200,
        #{<<"content-type">> => <<"application/json">>},
        Body, Req0),
    {ok, Req, State}.
