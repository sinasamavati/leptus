-module(leptus_router).
-author("Sina Samavati <sina.samv@gmail.com>").

-export([paths/1]).

-type handler() :: module().
-type route() :: cowboy_router:route_match().
-type state() :: any().

-record(ctx,
        {
          handler :: handler(),
          route :: route(),
          handler_state :: state()
        }).
-type ctx() :: #ctx{}.


-spec paths([{handler(), state()}]) -> cowboy_router:dispatch_rules().
paths(Handlers) ->
    handle_routes(fetch_routes(Handlers, []), []).

%% internal
-spec fetch_routes([{handler(), state()}], []) -> [ctx()].
fetch_routes([], Acc) ->
    Acc;
fetch_routes([{Handler, State}|T], Acc) ->
    %% each module must have routes/0 -> [string()].
    Ctx = [#ctx{handler=Handler, route=Route, handler_state=State}
           ||  Route <- Handler:routes()],
    fetch_routes(T, Acc ++ Ctx).

-spec handle_routes([ctx()], [none() | {route(), leptus_handler, ctx()}])
                   ->[cowboy_router:route_path()].
handle_routes([], Acc) ->
    Acc;
handle_routes([Ctx|T], Acc) ->
    handle_routes(T, Acc ++ [{Ctx#ctx.route, leptus_handler, Ctx}]).
