-module(leptus_routes1).

-export([routes/0]).


routes() ->
    ["/", "/blah", "/hello/:name", "/some-url/to/some-path"].
