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

-module(leptus_router_SUITE).

-export([groups/0]).
-export([all/0]).
-export([paths/1]).
-export([sort_dispatch/1]).
-export([static_file_routes/1]).

-include_lib("common_test/include/ct.hrl").
-include("src/leptus.hrl").

groups() ->
    [{main, [parallel], [paths, sort_dispatch, static_file_routes]}].

all() ->
    [{group, main}].

paths(_) ->
    Resrc1 = #resrc{handler=leptus_routes1, route="/", handler_state=[]},
    Resrc2 = #resrc{handler=leptus_routes1, route="/blah", handler_state=[]},
    Resrc3 = #resrc{handler=leptus_routes1, route="/hello/:name", handler_state=[]},
    Resrc4 = #resrc{handler=leptus_routes1, route="/some-url/to/some-path",
                    handler_state=[]},
    Resrc5 = #resrc{handler=leptus_routes2, route="/something/:key",
                    handler_state=aha},
    Resrc6 = #resrc{handler=leptus_routes2, route="/something/else",
                    handler_state=aha},
    Resrc7 = #resrc{handler=leptus_routes3, route="/users/:id",
                    handler_state=i_see},
    Resrc8 = #resrc{handler=leptus_routes3, route="/users/:id/info",
                    handler_state=i_see},
    Resrc9 = #resrc{handler=leptus_routes4, route="/items"},
    Resrc10 = #resrc{handler=leptus_routes4, route="/items/:id"},
    Resrc11 = #resrc{handler=leptus_routes4, route="/items/:id/childrens"},

    [{'_', [
            {"/", leptus_handler, Resrc1},
            {"/blah", leptus_handler, Resrc2},
            {"/hello/:name", leptus_handler, Resrc3},
            {"/some-url/to/some-path", leptus_handler, Resrc4},
            {"/something/:key", leptus_handler, Resrc5},
            {"/something/else", leptus_handler, Resrc6},
            {"/users/:id", leptus_handler, Resrc7},
            {"/users/:id/info", leptus_handler, Resrc8},
            {"/v1/items", leptus_handler, Resrc9},
            {"/v1/items/:id", leptus_handler, Resrc10},
            {"/v1/items/:id/childrens", leptus_handler, Resrc11}
           ]
     }] = leptus_router:paths([{'_', [{leptus_routes1, []},
                                      {leptus_routes2, aha},
                                      {leptus_routes3, i_see},
                                      {leptus_routes4, undefined}]}]),
    ok.

sort_dispatch(_) ->
    Routes = [
              {"/:bucket", handler, undefined},
              {"/[...]", handler, undefined},
              {"/_version", handler, undefined},
              {"/:bucket/:key", handler, undefined},
              {"/:bucket/_keys", handler, undefined},
              {"/_buckets", handler, undefined},
              {"/", handler, undefined}
             ],
    Dispatch = cowboy_router:compile([{'_', Routes}]),
    [
     {'_',[],
      [{[],[],handler,undefined},
       {[<<"_version">>],[],handler,undefined},
       {[<<"_buckets">>],[],handler,undefined},
       {[bucket],[],handler,undefined},
       {[bucket,<<"_keys">>],[],handler,undefined},
       {[bucket,key],[],handler,undefined},
       {['...'],[],handler,undefined}]}
    ] = leptus_router:sort_dispatch(Dispatch),

    Routes1 = [{"/:a/:b", handler, undefined},
               {"/:a/:b/x/:d", handler, undefined},
               {"/:a", handler, undefined},
               {"/:a/:b/:c", handler, undefined},
               {"/", handler, undefined},
               {"/:a/:b/x", handler, undefined},
               {"/:a/x/:c/:d", handler, undefined},
               {"/:a/x", handler, undefined}],
    Dispatch1 = cowboy_router:compile([{'_', Routes1}]),
    [{'_', [],
      [
       {[], [], handler, undefined},
       {[a], [], handler, undefined},
       {[a, <<"x">>], [], handler, undefined},
       {[a, b],[], handler, undefined},
       {[a, b, <<"x">>], [], handler, undefined},
       {[a, b, c], [], handler, undefined},
       {[a, b, <<"x">>, d], [], handler, undefined},
       {[a, <<"x">>, c, d], [], handler, undefined}
      ]}] = leptus_router:sort_dispatch(Dispatch1),
    ok.

static_file_routes(Conf) ->
    Dir = filename:join(?config(data_dir, Conf), "www"),
    Index = filename:join(Dir, "index.html"),
    Style = filename:join([Dir, "static", "css", "style.css"]),
    [{'_', [
            {"/", cowboy_static, {file, Index}},
            {"/index.html", cowboy_static, {file, Index}},
            {"/static/css/style.css", cowboy_static, {file, Style}}
           ]}] = leptus_router:static_file_routes({'_', Dir}),
    ok.
