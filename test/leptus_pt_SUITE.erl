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

-module(leptus_pt_SUITE).

-export([groups/0]).
-export([all/0]).
-export([routes/1]).
-export([allowed_methods/1]).
-export([rq_pt/1]).

%% helpers
-import(helpers, [request/2]).

groups() ->
    [{main, [parallel], [routes, allowed_methods, rq_pt]}].

all() ->
    [{group, main}].

routes(_) ->
    ["/", "/hello", "/hello/:name"] = pt1:routes(),
    ["/1", "/2", "/3", "/4"] = pt2:routes(),
    ["/", "/new", "/old"] = pt3:routes(),
    ok.

allowed_methods(_) ->
    [<<"GET">>] = pt1:allowed_methods("/"),
    [<<"GET">>] = pt1:allowed_methods("/hello"),
    [<<"GET">>] = pt1:allowed_methods("/hello/:name"),
    [<<"GET">>, <<"POST">>] = pt2:allowed_methods("/1"),
    [<<"GET">>] = pt3:allowed_methods("/"),
    [<<"POST">>] = pt3:allowed_methods("/new"),
    [<<"PUT">>, <<"DELETE">>] = pt3:allowed_methods("/old"),
    ok.

rq_pt(_) ->
    {ok, _} = pt3:start(),
    {200, _, _} = request(<<"GET">>, "/"),
    {200, _, _} = request(<<"PUT">>, "/old"),
    {201, _, _} = request(<<"POST">>, "/new"),
    {204, _, _} = request(<<"DELETE">>, "/old"),
    ok = pt3:stop().
