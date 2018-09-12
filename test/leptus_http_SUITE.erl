%% Copyright (c) 2013-2018 Sina Samavati <sina.samv@gmail.com>
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

%% helpers
-import(helpers, [request/2, request/3, request/4, response_body/1]).


init_per_suite(Config) ->
    Handlers = [
                {leptus_http1, []},
                {leptus_http2, []},
                {leptus_http3, []},
                {leptus_http4, []}
               ],
    Options = [{log_handlers, [{leptus_debug_log, default}]}],
    ok = application:ensure_started(inets),
    {ok, _} = application:ensure_all_started(leptus),
    {ok, _} = leptus:start_listener(http, [{'_', Handlers}], Options),
    Config.

end_per_suite(_Config) ->
    ok = leptus:stop_listener(http).

groups() ->
    [{main, [parallel], [
                         http_get,
                         http_post,
                         http_put,
                         http_delete,
                         http_404,
                         http_405,
                         http_is_authenticated,
                         http_has_permission
                        ]}].

all() ->
    [{group, main}].

http_get(_) ->
    M = get,
    {200, _, <<"index">>} = request(M, "/"),
    {200, _, <<"hello, world!">>} = request(M, "/hello"),
    {200, _, <<"hello, sina">>} = request(M, "/hello/sina"),
    {200, _, <<"aha, this is 1234">>} = request(M, "/users/1234"),
    {200, _, <<"art, photography...">>} = request(M, "/users/456/interests"),
    {200, _, <<"Erlang and", _/binary>>} = request(M, "/users/s1n4/interests"),
    {404, _, <<"not found...">>} = request(M, "/users/123/interests"),

    B1 = <<"{\"id\":\"asdf\",\"github\":\"asdf\",\"bio\":\"Erlanger\"}">>,
    B2 = <<"{\"id\":\"you\",\"github\":\"you\",\"bio\":\"Erlanger\"}">>,
    {200, _, B1} = request(M, "/users/asdf/profile"),
    {200, _, B2} = request(M, "/users/you/profile"),
    ok.

http_put(_) ->
    M = put,
    B1 = <<"password=lkjhgf&password_confirmation=lkjhg">>,
    {403, _, <<"Passwords didn't match.">>} =
        request(M, "/settings/change-password", [], B1),

    B2 = <<"password=lkjhgf&password_confirmation=lkjhgf">>,
    {200, _, <<"Your password has been changed.">>} =
        request(M, "/settings/change-password", [], B2),
    ok.

http_post(_) ->
    M = post,
    B1 = <<"username=asdf&email=asdf@a.<...>.com">>,
    {403, _, <<"Username is already taken.">>} =
        request(M, "/user/register", [], B1),

    B2 = <<"username=asdfg&email=something@a.<...>.com">>,
    {201, _, <<"Thanks for registration.">>} =
        request(M, "/user/register", [], B2),
    ok.

http_delete(_) ->
    M = delete,
    {404, _, _} = request(M, "/users/jack/posts/32601"),
    {404, _, _} = request(M, "/users/jack/posts/3268"),
    {204, _, _} = request(M, "/users/jack/posts/219").

http_404(_) ->
    {404, _, _} = request(get, "/asd"),
    {404, _, _} = request(get, "/asdf"),
    {404, _, _} = request(get, "/asdfg"),
    {404, _, _} = request(post, "/blah/new"),
    {404, _, _} = request(put, "/blah/186"),
    {404, _, _} = request(delete, "/blah/186"),
    {404, _, _} = request(head, "/blah/186"),
    ok.

http_405(_) ->
    {405, H1, _} = request(delete, "/users/876"),
    {405, H2, _} = request(delete, "/users/s1n4/interests"),
    {405, H3, _} = request(put, "/user/register"),
    {405, H4, _} = request(post, "/settings/change-password"),
    {405, H5, _} = request(get, "/user/register"),
    {405, H6, _} = request(head, "/users/876"),
    {405, H7, _} = request(head, "/users/blah/posts/876"),

    F = fun(H) -> proplists:get_value("allow", H) end,
    "GET, PUT, POST" = F(H1),
    "GET" = F(H2),
    "POST" = F(H3),
    "PUT" = F(H4),
    "POST" = F(H5),
    "GET, PUT, POST" = F(H6),
    "DELETE" = F(H7),
    ok.

http_is_authenticated(_) ->
    A1 = base64:encode(<<"123:456">>),
    A2 = base64:encode(<<"123:986">>),
    A3 = base64:encode(<<"sina:wrote_me">>),
    Auth = fun(D) -> [{"Authorization", "Basic " ++ binary_to_list(D)}] end,

    {401, _, _} = request(put, "/users/sina"),
    {401, _, _} = request(put, "/users/sina", Auth(A1)),
    {401, H0, <<"{\"error\":\"unauthorized\"}">>} =
        request(post, "/users/sina"),
    {401, H1, <<"{\"error\":\"unauthorized\"}">>} =
        request(post, "/users/sina", Auth(A2)),
    {200, _, _} = request(put, "/users/sina", Auth(A3)),
    {200, _, _} = request(post, "/users/sina", Auth(A3)),

    "application/json" = proplists:get_value("content-type", H0),
    "application/json" = proplists:get_value("content-type", H1),
    ok.

http_has_permission(_) ->
    Auth = fun(D) -> [{"Authorization", "Basic " ++ binary_to_list(D)}] end,
    A1 = Auth(base64:encode(<<"123:456">>)),
    A2 = Auth(base64:encode(<<"asdf:zxcv">>)),

    {403, _, <<"you don't have permission to do this shit!">>} =
        request(get, "/needs-perm", A1),
    {200, _, <<"hah, see you got permission">>} =
        request(get, "/needs-perm", A2),
    ok.
