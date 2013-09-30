%% @author Sina Samavati <sina.samv@gmail.com>

-module(leptus).
-author("Sina Samavati <sina.samv@gmail.com>").

-export([start_http/1]).


-spec start_http({modules, [module()]}) -> {ok, pid()} | {error, any()}.
start_http({modules, Mods}) ->
    ensure_started(crypto),
    ensure_started(ranch),
    ensure_started(cowboy),
    ensure_started(leptus),
    Paths = leptus_router:paths(Mods),
    Dispatch = cowboy_router:compile([{'_', Paths}]),
    {ok, _} = cowboy:start_http(
                http, 100, [{port, 8080}],
                [
                 {env, [{dispatch, Dispatch}]},
                 {onresponse, fun leptus_hooks:console_log/4}
                ]
               ).


%% internal
ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.
