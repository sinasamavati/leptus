-module(leptus_router).
-author("Sina Samavati <sina.samv@gmail.com>").

-export([paths/1]).

-type route() :: string().
-type routes_dict() :: [{module(), [route()]}].
-type routes_proplist() :: [{module(), route()}].

-spec paths([module()]) -> cowboy_router:dispatch_rules().
paths(Mods) ->
    Routes = fetch_routes(Mods, []),
    handle_routes(to_rp(Routes), []).

%% internal
-spec fetch_routes([module()], []) -> routes_dict().
fetch_routes([], Acc) ->
    Acc;
fetch_routes([Mod|T], Acc) ->
    %% each module must have routes/0 -> [string()].
    Routes = Mod:routes(),
    fetch_routes(T, orddict:append_list(Mod, Routes, Acc)).

-spec to_rp(routes_dict()) -> routes_proplist().
to_rp(RD) ->
    [{Mod, Route} || {Mod, Routes} <- RD, Route <- Routes].

-spec handle_routes(routes_proplist(), []) -> [cowboy_router:route_path()].
handle_routes([], Acc) ->
    Acc;
handle_routes([{Mod, Route}|T], Acc) ->
    handle_routes(T, Acc ++ [{Route, leptus_resouce_handler, {Mod, Route}}]).
