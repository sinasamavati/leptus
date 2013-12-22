-module(leptus_config_SUITE).

-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([all/0]).
-export([set/1]).
-export([lookup/1]).
-export([ip_addr/1]).
-export([port_num/1]).
-export([handlers/1]).


init_per_suite(Config) ->
    leptus_config:stop(), %% because it might be started in other suites
    gen_server:start({local, leptus_config}, leptus_config, []),
    Config.

end_per_suite(_Config) ->
    ok = leptus_config:stop().

all() ->
    [set, lookup, ip_addr, port_num, handlers].

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

ip_addr(_) ->
    {127, 0, 0, 1} = leptus_config:ip_addr(http),

    leptus_config:set(http, [{ip, {0, 0, 0, 0}}]),
    {0, 0, 0, 0} = leptus_config:ip_addr(http),

    leptus_config:set(http, [{ip, undefined}]),
    {127, 0, 0, 1} = leptus_config:ip_addr(http),

    leptus_config:set(https, [{ip, {10, 10, 0, 1}}]),
    {10, 10, 0, 1} = leptus_config:ip_addr(https),

    leptus_config:set(spdy, [{ip, {10, 10, 0, 5}}]),
    {10, 10, 0, 5} = leptus_config:ip_addr(spdy),

    {127, 0, 0, 1} = leptus_config:ip_addr(http),
    {10, 10, 0, 1} = leptus_config:ip_addr(https).

port_num(_) ->
    8080 = leptus_config:port_num(http),

    leptus_config:set(http, [{port, 9000}]),
    9000 = leptus_config:port_num(http),

    leptus_config:set(http, [{port, undefined}]),
    8080 = leptus_config:port_num(http),

    leptus_config:set(https, [{port, 4443}]),
    4443 = leptus_config:port_num(https),

    leptus_config:set(spdy, [{port, 9876}]),
    9876 = leptus_config:port_num(spdy),

    8080 = leptus_config:port_num(http),
    4443 = leptus_config:port_num(https).

handlers(_) ->
    [] = leptus_config:handlers(),

    leptus_config:set(handlers, [{rh1, undefined_state}]),
    [{rh1, undefined_state}] = leptus_config:handlers(),

    leptus_config:set(handlers, [{rh1, undefined_state}, {rh2, nothing}]),
    [{rh1, undefined_state}, {rh2, nothing}] = leptus_config:handlers(),

    leptus_config:set(handlers, []),
    [] = leptus_config:handlers().
