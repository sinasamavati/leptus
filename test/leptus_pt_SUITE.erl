-module(leptus_pt_SUITE).

-export([all/0]).
-export([routes/1]).
-export([allowed_methods/1]).
-export([rq_pt/1]).

%% helpers
-import(helpers, [request/2]).


all() ->
    [routes, allowed_methods, rq_pt].

routes(_) ->
    ["/", "/hello", "/hello/:name"] = pt1:routes(),
    ["/1", "/2", "/3", "/4"] = pt2:routes(),
    ["/", "/new", "/old"] = pt3:routes().

allowed_methods(_) ->
    <<"GET">> = pt1:allowed_methods("/"),
    <<"GET">> = pt1:allowed_methods("/hello"),
    <<"GET">> = pt1:allowed_methods("/hello/:name"),
    <<"GET, POST">> = pt2:allowed_methods("/1"),
    <<"GET">> = pt3:allowed_methods("/"),
    <<"POST">> = pt3:allowed_methods("/new"),
    <<"PUT, DELETE">> = pt3:allowed_methods("/old").

rq_pt(_) ->
    {ok, _} = pt3:start(),
    ["/", "/new", "/old"] = pt3:routes(),
    {200, _, _} = request(<<"GET">>, "/"),
    {200, _, _} = request(<<"PUT">>, "/old"),
    {201, _, _} = request(<<"POST">>, "/new"),
    {204, _, _} = request(<<"DELETE">>, "/old"),
    ok = leptus:stop_http().
