-module(leptus_http_SUITE).

-export([init_per_suite/1]).
-export([all/0]).

-export([http_get/1]).
-export([http_404/1]).


init_per_suite(Config) ->
    {ok, _} = leptus:start_http({modules, [leptus_http1]}),
    Config.

all() ->
    [http_get, http_404].


http_get(_) ->
    {ok, 200, _, C1} = hackney:get("http://localhost:8080/"),
    {ok, <<"index">>, _} = hackney:body(C1),

    {ok, 200, _, C2} = hackney:get("http://localhost:8080/hello"),
    {ok, <<"hello, world!">>, _} = hackney:body(C2),

    {ok, 200, _, C3} = hackney:get("http://localhost:8080/hello/sina"),
    {ok, <<"hello, sina">>, _} = hackney:body(C3).

http_404(_) ->
    {ok, 404, _, _} = hackney:get("http://localhost:8080/asd"),
    {ok, 404, _, _} = hackney:get("http://localhost:8080/asdf"),
    {ok, 404, _, _} = hackney:get("http://localhost:8080/asdfg").
