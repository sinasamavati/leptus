-module(leptus_http1).

-export([routes/0]).
-export([get/2]).

routes() ->
    ["/", "/hello", "/hello/:name"].

get("/", _Req) ->
    {200, "index"};

get("/hello", _Req) ->
    {200, "hello, world!"};

get("/hello/:name", Req) ->
    Name = leptus_req:binding(name, Req),
    {200, "hello, " ++ Name}.
