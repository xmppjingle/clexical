-include("../include/clexical.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("confetti/include/confetti.hrl").

-define(PRED(ID,SUB,ACT,ABS), #predicate{id= <<ID>>, subject= <<SUB>>, action=ACT, abstract=ABS}).

-define(start_lager(), begin
    case lists:keyfind(lager, 1, application:loaded_applications()) of
        false ->
            lager:start(),
            lager:set_loglevel(lager_console_backend, debug);
        _ ->
            ok
    end    
end).

-define(start_redo(), begin
    case lists:keyfind(redo, 1, application:loaded_applications()) of
        false ->
            redo:start_link();
        _ ->
            ok
    end    
end).

-define(start_exmpp(), begin
    case lists:keyfind(redo, 1, application:loaded_applications()) of
        false ->
            redo:start_link();
        _ ->
            ok
    end    
end).

-define(meck_confetti(Config), begin
    case whereis(confetti) of
        undefined ->
            meck:new(confetti),
            meck:expect(confetti, fetch, 1, Config);
        _ ->
            ok
    end
end).
