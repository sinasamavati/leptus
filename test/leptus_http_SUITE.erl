-module(leptus_http_SUITE).

-export([init_per_suite/1]).
-export([all/0]).

-export([http_get/1]).
-export([http_404/1]).
-export([http_405/1]).
-export([http_post/1]).


init_per_suite(Config) ->
    {ok, _} =
        leptus:start_http(
          {modules,
           [leptus_http1, leptus_http2, leptus_http3]
          }),
    Config.

all() ->
    [http_get, http_404, http_405, http_post].


http_get(_) ->
    {ok, 200, _, C1} = hackney:get("localhost:8080/"),
    {ok, <<"index">>, _} = hackney:body(C1),

    {ok, 200, _, C2} = hackney:get("localhost:8080/hello"),
    {ok, <<"hello, world!">>, _} = hackney:body(C2),

    {ok, 200, _, C3} = hackney:get("localhost:8080/hello/sina"),
    {ok, <<"hello, sina">>, _} = hackney:body(C3),

    {ok, 200, _, C4} = hackney:get("localhost:8080/users/1234"),
    {ok, <<"aha, this is 1234">>, _} = hackney:body(C4),

    {ok, 200, _, C5} = hackney:get("localhost:8080/users/456/interests"),
    {ok, <<"art, photography...">>, _} = hackney:body(C5),

    {ok, 200, _, C6} = hackney:get("localhost:8080/users/s1n4/interests"),
    {ok, <<"Erlang and a lotta things else">>, _} = hackney:body(C6),

    {ok, 404, _, C7} = hackney:get("localhost:8080/users/123/interests"),
    {ok, <<"not found...">>, _} = hackney:body(C7),

    B1 = <<"{\"id\":\"asdf\",\"bio\":\"Erlanger\",\"github\":\"asdf\"}">>,
    B2 = <<"{\"id\":\"you\",\"bio\":\"Erlanger\",\"github\":\"you\"}">>,
    true = jsx:is_json(B1),
    true = jsx:is_json(B2),

    {ok, 200, _, C8} = hackney:get("localhost:8080/users/asdf/profile"),
    {ok, B1, _} = hackney:body(C8),
    {ok, 200, _, C9} = hackney:get("localhost:8080/users/you/profile"),
    {ok, B2, _} = hackney:body(C9).

http_404(_) ->
    {ok, 404, _, _} = hackney:get("localhost:8080/asd"),
    {ok, 404, _, _} = hackney:get("localhost:8080/asdf"),
    {ok, 404, _, _} = hackney:get("localhost:8080/asdfg").

http_405(_) ->
    {ok, 405, _, _} = hackney:delete("localhost:8080/users/876"),
    {ok, 405, _, _} = hackney:delete("localhost:8080/users/s1n4/interests").

http_post(_) ->
    B1 = <<"username=asdf&email=asdf@a.<...>.com">>,
    {ok, 403, _, C1} = hackney:post("localhost:8080/user/register", [], B1),
    {ok, <<"Username is already taken.">>, _} = hackney:body(C1),

    B2 = <<"username=asdfg&email=something@a.<...>.com">>,
    {ok, 201, _, C2} = hackney:post("localhost:8080/user/register", [], B2),
    {ok, <<"Thanks for registration.">>, _} = hackney:body(C2).
