-module(leptus_router_SUITE).

-export([all/0]).
-export([paths/1]).


all() ->
    [paths].

paths(_) ->
    [
     {"/",
      leptus_resouce_handler, {leptus_routes1, "/"}
     },

     {"/blah",
      leptus_resouce_handler, {leptus_routes1, "/blah"}
     },

     {"/hello/:name",
      leptus_resouce_handler, {leptus_routes1, "/hello/:name"}
     },

     {"/some-url/to/some-path",
      leptus_resouce_handler, {leptus_routes1, "/some-url/to/some-path"}
     },

     {"/something/:key",
      leptus_resouce_handler, {leptus_routes2, "/something/:key"}
     },

     {"/something/else",
      leptus_resouce_handler, {leptus_routes2, "/something/else"}
     },

     {"/users/:id",
      leptus_resouce_handler, {leptus_routes3, "/users/:id"}
     },

     {"/users/:id/info",
      leptus_resouce_handler, {leptus_routes3, "/users/:id/info"}
     }
    ] = leptus_router:paths([leptus_routes1, leptus_routes2, leptus_routes3]).
