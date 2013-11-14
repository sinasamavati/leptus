-module(leptus_pt_SUITE).

-export([all/0]).
-export([routes/1]).
-export([allowed_methods/1]).
-export([rq_pt/1]).


all() ->
    [routes, allowed_methods, rq_pt].

routes(_) ->
    ["/", "/hello", "/hello/:name"] = pt1:routes(),
    ["/1", "/2", "/3", "/4"] = pt2:routes(),
    ["/", "/new", "/old"] = pt3:routes().

allowed_methods(_) ->
    [<<"GET">>] = pt1:allowed_methods("/"),
    [<<"GET">>] = pt1:allowed_methods("/hello"),
    [<<"GET">>] = pt1:allowed_methods("/hello/:name"),
    [<<"GET">>, <<"POST">>] = pt2:allowed_methods("/1"),
    [<<"GET">>] = pt3:allowed_methods("/"),
    [<<"POST">>] = pt3:allowed_methods("/new"),
    [<<"PUT">>, <<"DELETE">>] = pt3:allowed_methods("/old").

rq_pt(_) ->
    {ok, _} = leptus:start_http({modules, [pt3]}),
    ["/", "/new", "/old"] = pt3:routes(),
    {ok, 200, _, _} = hackney:get("localhost:8080/"),
    {ok, 200, _, _} = hackney:put("localhost:8080/old"),
    {ok, 201, _, _} = hackney:post("localhost:8080/new"),
    {ok, 204, _, _} = hackney:delete("localhost:8080/old"),
    ok = leptus:stop_http().
