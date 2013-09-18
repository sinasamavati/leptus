%% @author Sina Samavati <sina.samv@gmail.com>

-module(leptus).
-author("Sina Samavati <sina.samv@gmail.com>").

-export([start_http/1]).


ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.

start_http({modules, Mods}) ->
    ensure_started(crypto),
    ensure_started(ranch),
    ensure_started(cowboy),
    Dispatch = cowboy_router:compile(routes(Mods)),
    {ok, _} = cowboy:start_http(http, 100, [{port, 8080}],
                                [{env, [{dispatch, Dispatch}]}]).

routes(Mods) ->
    [{'_', handle_routes(Mods)}].

handle_routes(Mods) ->
    Routes = find_routes(Mods),
    handle_url_patterns(Routes).

find_routes(Mods) ->
    find_routes(Mods, []).

find_routes([], Acc) ->
    Acc;
find_routes([Mod|T], Acc) ->
    Routes = apply(Mod, routes, []),
    find_routes(T, Acc ++ Routes).

handle_url_patterns(Routes) ->
    handle_url_patterns(Routes, []).

handle_url_patterns([], Acc) ->
    Acc;
handle_url_patterns([Route|T], Acc) ->
    handle_url_patterns(T, Acc ++ [{Route, leptus_resouce_handler, Route}]).
