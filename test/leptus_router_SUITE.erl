-module(leptus_router_SUITE).

-export([all/0]).
-export([fetch_routes/1]).
-export([dispatches/1]).
-export([routes/0]).


all() ->
    [fetch_routes, dispatches].

fetch_routes(_) ->
    [{?MODULE,
      ["/", "/blah", "/hello/:name", "/some-url/to/some-path"]
     }] = leptus_router:fetch_routes([?MODULE]).

dispatches(_) ->
    [{'_', [
            {"/", leptus_resouce_handler, "/"},
            {"/blah", leptus_resouce_handler, "/blah"},
            {"/hello/:name", leptus_resouce_handler, "/hello/:name"},
            {"/some-url/to/some-path", leptus_resouce_handler,
             "/some-url/to/some-path"}
           ]
     }] = leptus_router:dispatches([?MODULE]).


%% API
routes() ->
    ["/", "/blah", "/hello/:name", "/some-url/to/some-path"].
