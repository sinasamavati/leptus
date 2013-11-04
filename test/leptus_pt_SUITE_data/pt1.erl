-module(pt1).
-compile({parse_transform, leptus_pt}).
-export([get/2]).

get("/", _Req) ->
    {200, <<>>};
get("/hello", _Req) ->
    {200, <<"hello">>};
get("/hello/:name", _Req) ->
    {200, <<>>}.
