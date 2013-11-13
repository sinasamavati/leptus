-module(leptus_router).
-author("Sina Samavati <sina.samv@gmail.com>").

-export([paths/1]).

-type route() :: string().
-type routes_proplist() :: [{module(), route()}].

-spec paths([module()]) -> cowboy_router:dispatch_rules().
paths(Mods) ->
    handle_routes(fetch_routes(Mods, []), []).

%% internal
-spec fetch_routes([module()], []) -> routes_proplist().
fetch_routes([], Acc) ->
    Acc;
fetch_routes([Mod|T], Acc) ->
    %% each module must have routes/0 -> [string()].
    fetch_routes(T, Acc ++ [{Mod, Route} || Route <- Mod:routes()]).

-spec handle_routes(routes_proplist(), []) -> [cowboy_router:route_path()].
handle_routes([], Acc) ->
    Acc;
handle_routes([{Mod, Route}|T], Acc) ->
    handle_routes(T, Acc ++ [{Route, leptus_resouce_handler, {Mod, Route}}]).
