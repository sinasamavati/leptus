-module(leptus_http_SUITE).

-export([all/0]).
-export([http_get/1]).


all() ->
    [http_get].

http_get(_) ->
    {ok, _} = leptus:start_http({modules, [leptus_http1]}),

    {ok, 200, _, C1} = hackney:get("http://localhost:8080/"),
    {ok, <<"index">>, _} = hackney:body(C1),

    {ok, 200, _, C2} = hackney:get("http://localhost:8080/hello"),
    {ok, <<"hello, world!">>, _} = hackney:body(C2),

    {ok, 200, _, C3} = hackney:get("http://localhost:8080/hello/sina"),
    {ok, <<"hello, sina">>, _} = hackney:body(C3).
