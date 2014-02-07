%% This file is part of leptus, and released under the MIT license.
%% See LICENSE for more information.

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
    [{port_num, 8080}, {ip_addr, "127.0.0.1"}] = leptus_config:lookup(http).
