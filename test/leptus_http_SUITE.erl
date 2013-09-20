-module(leptus_http_SUITE).

-export([init_per_suite/1]).
-export([all/0]).

-export([http_get/1]).
-export([http_404/1]).
-export([http_405/1]).


init_per_suite(Config) ->
    {ok, _} = leptus:start_http({modules, [leptus_http1, leptus_http2]}),
    Config.

all() ->
    [http_get, http_404, http_405].


http_get(_) ->
    {ok, 200, _, C1} = hackney:get("http://localhost:8080/"),
    {ok, <<"index">>, _} = hackney:body(C1),

    {ok, 200, _, C2} = hackney:get("http://localhost:8080/hello"),
    {ok, <<"hello, world!">>, _} = hackney:body(C2),

    {ok, 200, _, C3} = hackney:get("http://localhost:8080/hello/sina"),
    {ok, <<"hello, sina">>, _} = hackney:body(C3),

    {ok, 200, _, C4} = hackney:get("http://localhost:8080/users/1234"),
    {ok, <<"aha, this is 1234">>, _} = hackney:body(C4),

    {ok, 200, _, C5} = hackney:get("http://localhost:8080/users/1234/interests"),
    {ok, <<"art, photography...">>, _} = hackney:body(C5),

    {ok, 200, _, C6} = hackney:get("http://localhost:8080/users/s1n4/interests"),
    {ok, <<"Erlang and a lotta things else">>, _} = hackney:body(C6),

    {ok, 404, _, C7} = hackney:get("http://localhost:8080/users/456/interests"),
    {ok, <<"not found...">>, _} = hackney:body(C7).

http_404(_) ->
    {ok, 404, _, _} = hackney:get("http://localhost:8080/asd"),
    {ok, 404, _, _} = hackney:get("http://localhost:8080/asdf"),
    {ok, 404, _, _} = hackney:get("http://localhost:8080/asdfg").


http_405(_) ->
    {ok, 405, _, _} = hackney:delete("http://localhost:8080/users/876"),
    {ok, 405, _, _} = hackney:delete("http://localhost:8080/users/s1n4/interests").
