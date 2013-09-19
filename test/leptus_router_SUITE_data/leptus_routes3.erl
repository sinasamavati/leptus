-module(leptus_routes3).

-export([routes/0]).


routes() ->
    ["/users/:id", "/users/:id/info"].
