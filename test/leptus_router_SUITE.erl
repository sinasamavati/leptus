-module(leptus_router_SUITE).

-export([all/0]).
-export([dispatches/1]).
-export([routes/0]).


all() ->
    [dispatches].

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
