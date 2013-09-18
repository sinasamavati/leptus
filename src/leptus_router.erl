-module(leptus_router).

-export([dispatches/1]).


-spec dispatches([module()]) -> cowboy_router:dispatch_rules().
dispatches(Mods) ->
    [{'_', fetch_paths(Mods)}].

-spec fetch_paths([module()]) -> [cowboy_router:route_path()].
fetch_paths(Mods) ->
    Routes = fetch_routes(Mods),
    handle_routes(Routes).

-spec fetch_routes([module()]) -> [string()].
fetch_routes(Mods) ->
    fetch_routes(Mods, []).

fetch_routes([], Acc) ->
    Acc;
fetch_routes([Mod|T], Acc) ->
    %% each module must have routes/0 -> [string()].
    Routes = apply(Mod, routes, []),
    fetch_routes(T, Acc ++ Routes).

-spec handle_routes([string()]) -> [cowboy_router:route_path()].
handle_routes(Routes) ->
    handle_routes(Routes, []).

handle_routes([], Acc) ->
    Acc;
handle_routes([Route|T], Acc) ->
    handle_routes(T, Acc ++ [{Route, leptus_resouce_handler, Route}]).
