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

-module(leptus_config_SUITE).

-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([all/0]).
-export([set/1]).
-export([lookup/1]).


init_per_suite(Config) ->
    leptus_config:stop(), %% because it might be started in other suites
    leptus_config:start(),
    Config.

end_per_suite(_Config) ->
    ok = leptus_config:stop().

all() ->
    [set, lookup].

set(_) ->
    ok = leptus_config:set(t1, <<1,2,3>>),
    ok = leptus_config:set(t2, <<"1,2,3">>),
    ok = leptus_config:set(http1, {port_num, 8080}),
    ok = leptus_config:set(http2, {ip_addr, "127.0.0.1"}),
    ok = leptus_config:set(http, [{port_num, 8080}, {ip_addr, "127.0.0.1"}]).

lookup(_) ->
    <<1,2,3>> = leptus_config:lookup(t1),
    <<"1,2,3">> = leptus_config:lookup(t2),
    {port_num, 8080} = leptus_config:lookup(http1),
    {ip_addr, "127.0.0.1"} = leptus_config:lookup(http2),
    [{port_num, 8080}, {ip_addr, "127.0.0.1"}] = leptus_config:lookup(http),
    ok.
