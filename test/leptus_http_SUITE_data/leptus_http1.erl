-module(leptus_http1).

-export([routes/0]).
-export([get/2]).

routes() ->
    ["/", "/hello", "/hello/:name"].

get("/", _Req) ->
    {ok, 200, "index"};

get("/hello", _Req) ->
    {ok, 200, "hello, world!"};

get("/hello/:name", Req) ->
    {Name, _} = cowboy_req:binding(name, Req),
    {ok, 200, "hello, " ++ Name}.
