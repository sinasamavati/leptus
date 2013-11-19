-module(leptus_router_SUITE).

-export([all/0]).
-export([paths/1]).

-record(ctx, {handler, route, handler_state}).


all() ->
    [paths].

paths(_) ->
    Ctx1 = #ctx{handler=leptus_routes1, route="/", handler_state=[]},
    Ctx2 = #ctx{handler=leptus_routes1, route="/blah", handler_state=[]},
    Ctx3 = #ctx{handler=leptus_routes1, route="/hello/:name", handler_state=[]},
    Ctx4 = #ctx{handler=leptus_routes1, route="/some-url/to/some-path",
                handler_state=[]},
    Ctx5 = #ctx{handler=leptus_routes2, route="/something/:key",
                handler_state=aha},
    Ctx6 = #ctx{handler=leptus_routes2, route="/something/else",
                handler_state=aha},
    Ctx7 = #ctx{handler=leptus_routes3, route="/users/:id",
                handler_state=i_see},
    Ctx8 = #ctx{handler=leptus_routes3, route="/users/:id/info",
                handler_state=i_see},
    [
     {"/", leptus_handler, Ctx1},
     {"/blah", leptus_handler, Ctx2},
     {"/hello/:name", leptus_handler, Ctx3},
     {"/some-url/to/some-path", leptus_handler, Ctx4},
     {"/something/:key", leptus_handler, Ctx5},
     {"/something/else", leptus_handler, Ctx6},
     {"/users/:id", leptus_handler, Ctx7},
     {"/users/:id/info", leptus_handler, Ctx8}
    ] = leptus_router:paths([{leptus_routes1, []}, {leptus_routes2, aha}, {leptus_routes3, i_see}]).
