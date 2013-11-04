-module(leptus_pt_SUITE).

-export([all/0]).
-export([pt/1]).
-export([rq_pt/1]).


all() ->
    [pt, rq_pt].

pt(_) ->
    ["/", "/hello", "/hello/:name"] = pt1:routes(),
    ["/1", "/2", "/3", "/4"] = pt2:routes().

rq_pt(_) ->
    {ok, _} = leptus:start_http({modules, [pt3]}),
    ["/", "/new", "/old"] = pt3:routes(),
    {ok, 200, _, _} = hackney:get("localhost:8080/"),
    {ok, 200, _, _} = hackney:put("localhost:8080/old"),
    {ok, 201, _, _} = hackney:post("localhost:8080/new"),
    {ok, 204, _, _} = hackney:delete("localhost:8080/old"),
    ok = leptus:stop_http().
