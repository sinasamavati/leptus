-module(leptus_routes2).

-export([routes/0]).


routes() ->
    ["/something/:key", "/something/else"].
