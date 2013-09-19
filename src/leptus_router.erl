-module(leptus_router).

-export([fetch_routes/1]).
-export([dispatches/1]).

-type routes() :: [{module(), [string()]}].


-spec fetch_routes([module()]) -> routes().
fetch_routes(Mods) ->
    fetch_routes(Mods, []).

fetch_routes([], Acc) ->
    Acc;
fetch_routes([Mod|T], Acc) ->
    %% each module must have routes/0 -> [string()].
    Routes = apply(Mod, routes, []),
    fetch_routes(T, orddict:append_list(Mod, Routes, Acc)).

-spec dispatches([module()]) -> cowboy_router:dispatch_rules().
dispatches(Mods) ->
    [{'_', fetch_paths(Mods)}].

-spec fetch_paths([module()]) -> [cowboy_router:route_path()].
fetch_paths(Mods) ->
    Routes = fetch_routes(Mods),
    handle_routes(Routes).

-spec handle_routes(routes()) -> [cowboy_router:route_path()].
handle_routes(Routes) ->
    Values = orddict:fold(fun(_, V, AccIn) -> AccIn ++ V end, [], Routes),
    handle_routes(Values, []).

handle_routes([], Acc) ->
    Acc;
handle_routes([Route|T], Acc) ->
    handle_routes(T, Acc ++ [{Route, leptus_resouce_handler, Route}]).
