-module(pt1).
-compile({parse_transform, leptus_pt}).
-export([get/3]).

get("/", _Req, State) ->
    {200, <<>>, State};
get("/hello", _Req, State) ->
    {200, <<"hello">>, State};
get("/hello/:name", _Req, State) ->
    {200, <<>>, State}.
