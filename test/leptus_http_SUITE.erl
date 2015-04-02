%% Copyright (c) 2013-2015 Sina Samavati <sina.samv@gmail.com>
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.

-module(leptus_http_SUITE).

%% Common Test callbacks
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([groups/0]).
-export([all/0]).

%% test cases
-export([http_get/1]).
-export([http_put/1]).
-export([http_post/1]).
-export([http_delete/1]).
-export([http_404/1]).
-export([http_405/1]).
-export([http_is_authenticated/1]).
-export([http_has_permission/1]).
-export([http_msgpack/1]).

%% helpers
-import(helpers, [request/2, request/3, request/4, response_body/1]).


init_per_suite(Config) ->
    Handlers = [
                {leptus_http1, []},
                {leptus_http2, []},
                {leptus_http3, []},
                {leptus_http4, []},
                {leptus_http5, []}
               ],
    Options = [{log_handlers, [{leptus_debug_log, default}]}],
    {ok, _} = leptus:start_listener(http, [{'_', Handlers}], Options),
    Config.

end_per_suite(_Config) ->
    ok = leptus:stop_listener(http).

groups() ->
    [{main, [parallel], [http_get,
                         http_post,
                         http_put,
                         http_delete,
                         http_404,
                         http_405,
                         http_is_authenticated,
                         http_has_permission,
                         http_msgpack]}].

all() ->
    [{group, main}].

http_get(_) ->
    M = <<"GET">>,

    {200, _, C1} = request(M, "/"),
    <<"index">>= response_body(C1),

    {200, _, C2} = request(M, "/hello"),
    <<"hello, world!">> = response_body(C2),

    {200, _, C3} = request(M, "/hello/sina"),
    <<"hello, sina">> = response_body(C3),

    {200, _, C4} = request(M, "/users/1234"),
    <<"aha, this is 1234">> = response_body(C4),

    {200, _, C5} = request(M, "/users/456/interests"),
    <<"art, photography...">> = response_body(C5),

    {200, _, C6} = request(M, "/users/s1n4/interests"),
    <<"Erlang and a lotta things else">> = response_body(C6),

    {404, _, C7} = request(M, "/users/123/interests"),
    <<"not found...">> = response_body(C7),

    B1 = <<"{\"id\":\"asdf\",\"bio\":\"Erlanger\",\"github\":\"asdf\"}">>,
    B2 = <<"{\"id\":\"you\",\"bio\":\"Erlanger\",\"github\":\"you\"}">>,

    {200, _, C8} = request(M, "/users/asdf/profile"),
    B1 = response_body(C8),
    {200, _, C9} = request(M, "/users/you/profile"),
    B2 = response_body(C9),

    B3 = <<129,163,109,115,103,168,119,104,97,116,101,118,101,114>>,
    {200, _, C10} = request(M, "/msgpack/whatever"),
    B3 = response_body(C10).

http_put(_) ->
    M = <<"PUT">>,
    B1 = <<"password=lkjhgf&password_confirmation=lkjhg">>,
    {403, _, C1} = request(M, "/settings/change-password", [], B1),
    <<"Passwords didn't match.">> = response_body(C1),

    B2 = <<"password=lkjhgf&password_confirmation=lkjhgf">>,
    {200, _, C2} = request(M, "/settings/change-password", [], B2),
    <<"Your password has been changed.">> = response_body(C2).

http_post(_) ->
    M = <<"POST">>,
    B1 = <<"username=asdf&email=asdf@a.<...>.com">>,
    {403, _, C1} = request(M, "/user/register", [], B1),
    <<"Username is already taken.">> = response_body(C1),

    B2 = <<"username=asdfg&email=something@a.<...>.com">>,
    {201, _, C2} = request(M, "/user/register", [], B2),
    <<"Thanks for registration.">> = response_body(C2).

http_delete(_) ->
    M = <<"DELETE">>,
    {404, _, _} = request(M, "/users/jack/posts/32601"),
    {404, _, _} = request(M, "/users/jack/posts/3268"),
    {204, _, _} = request(M, "/users/jack/posts/219").

http_404(_) ->
    {404, _, _} = request(<<"GET">>, "/asd"),
    {404, _, _} = request(<<"GET">>, "/asdf"),
    {404, _, _} = request(<<"GET">>, "/asdfg"),
    {404, _, _} = request(<<"POST">>, "/blah/new"),
    {404, _, _} = request(<<"PUT">>, "/blah/186"),
    {404, _, _} = request(<<"DELETE">>, "/blah/186"),
    {404, _, _} = request(<<"HEAD">>, "/blah/186").

http_405(_) ->
    {405, H1, _} = request(<<"DELETE">>, "/users/876"),
    {405, H2, _} = request(<<"DELETE">>, "/users/s1n4/interests"),
    {405, H3, _} = request(<<"PUT">>, "/user/register"),
    {405, H4, _} = request(<<"POST">>, "/settings/change-password"),
    {405, H5, _} = request(<<"GET">>, "/user/register"),
    {405, H6, _} = request(<<"HEAD">>, "/users/876"),
    {405, H7, _} = request(<<"HEAD">>, "/users/blah/posts/876"),

    F = fun(H) -> proplists:get_value(<<"allow">>, H) end,
    <<"GET, PUT, POST">> = F(H1),
    <<"GET">> = F(H2),
    <<"POST">> = F(H3),
    <<"PUT">> = F(H4),
    <<"POST">> = F(H5),
    <<"GET, PUT, POST">> = F(H6),
    <<"DELETE">> = F(H7).

http_is_authenticated(_) ->
    A1 = base64:encode(<<"123:456">>),
    A2 = base64:encode(<<"123:986">>),
    A3 = base64:encode(<<"sina:wrote_me">>),
    Auth = fun(D) -> [{<<"Authorization">>, <<"Basic ", D/binary>>}] end,

    {401, _, _} = request(<<"PUT">>, "/users/sina"),
    {401, _, _} = request(<<"PUT">>, "/users/sina", Auth(A1)),
    {401, H, C} = request(<<"POST">>, "/users/sina"),
    {401, H1, C1} = request(<<"POST">>, "/users/sina", Auth(A2)),
    {200, _, _} = request(<<"PUT">>, "/users/sina", Auth(A3)),
    {200, _, _} = request(<<"POST">>, "/users/sina", Auth(A3)),

    <<"application/json">> = proplists:get_value(<<"content-type">>, H),
    <<"{\"error\":\"unauthorized\"}">> = response_body(C),
    <<"application/json">> = proplists:get_value(<<"content-type">>, H1),
    <<"{\"error\":\"unauthorized\"}">> = response_body(C1).

http_has_permission(_) ->
    Auth = fun(D) -> [{<<"Authorization">>, <<"Basic ", D/binary>>}] end,
    A1 = Auth(base64:encode(<<"123:456">>)),
    A2 = Auth(base64:encode(<<"asdf:zxcv">>)),

    {403, _, C1} = request(<<"GET">>, "/needs-perm", A1),
    {200, _, C2} = request(<<"GET">>, "/needs-perm", A2),
    <<"you don't have permission to do this shit!">> = response_body(C1),
    <<"hah, see you got permission">> = response_body(C2).

http_msgpack(_) ->
    M = <<"GET">>,
    ContentType = <<"application/x-msgpack">>,
    B1 = <<129,163,109,115,103,168,119,104,97,116,101,118,101,114>>,
    B2 = <<129,163,109,115,103,181,99,111,111,108,45,104,109,109,
           45,109,115,103,112,97,99,107,45,99,111,111,108>>,

    {200, H1, C1} = request(M, "/msgpack/whatever"),
    {200, H2, C2} = request(M, "/msgpack/cool-hmm-msgpack-cool"),

    B1 = response_body(C1),
    B2 = response_body(C2),
    ContentType = proplists:get_value(<<"content-type">>, H1),
    ContentType = proplists:get_value(<<"content-type">>, H2).
