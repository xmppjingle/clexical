-include("../include/clexical.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(start_lager(), begin
    case lists:keyfind(lager, 1, application:loaded_applications()) of
        false ->
            lager:start(),
            lager:set_loglevel(lager_console_backend, debug);
        _ ->
            ok
    end    
end).

-define(start_apps(), begin
    case lists:keyfind(fast_xml, 1, application:loaded_applications()) of
        false ->
            application:start(fast_xml);
        _ ->
            ok
    end,
    case lists:keyfind(mnesia, 1, application:loaded_applications()) of
        false ->
            application:start(mnesia);
        _ ->
            ok
    end   
end).