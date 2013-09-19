-module(leptus_router_SUITE).

-export([all/0]).
-export([fetch_routes/1]).
-export([dispatches/1]).


all() ->
    [fetch_routes, dispatches].

fetch_routes(_) ->
    [{leptus_routes1,
      ["/", "/blah", "/hello/:name", "/some-url/to/some-path"]
     },
     {leptus_routes2,
      ["/something/:key", "/something/else"]
     },
     {leptus_routes3,
      ["/users/:id", "/users/:id/info"]
     }
    ] = leptus_router:fetch_routes([leptus_routes1, leptus_routes2,
                                      leptus_routes3]).

dispatches(_) ->
    [{'_', [
            {"/", leptus_resouce_handler, "/"},
            {"/blah", leptus_resouce_handler, "/blah"},
            {"/hello/:name", leptus_resouce_handler, "/hello/:name"},
            {"/some-url/to/some-path", leptus_resouce_handler,
             "/some-url/to/some-path"},
            {"/something/:key", leptus_resouce_handler, "/something/:key"},
            {"/something/else", leptus_resouce_handler, "/something/else"},
            {"/users/:id", leptus_resouce_handler, "/users/:id"},
            {"/users/:id/info", leptus_resouce_handler, "/users/:id/info"}
           ]
     }] = leptus_router:dispatches([leptus_routes1, leptus_routes2,
                                    leptus_routes3]).
