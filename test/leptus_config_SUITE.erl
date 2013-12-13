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
    case leptus_config:start_link() of
        {ok, Pid} ->
            unlink(Pid);
        {error, {already_started, Pid}} ->
            unlink(Pid)
    end,
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
    {127, 0, 0, 1} = leptus_config:ip_addr(),

    leptus_config:set(http, [{ip, {0, 0, 0, 0}}]),
    {0, 0, 0, 0} = leptus_config:ip_addr(),

    leptus_config:set(http, [{ip, {10, 10, 0, 1}}]),
    {10, 10, 0, 1} = leptus_config:ip_addr(),

    leptus_config:set(http, [{ip, undefined}]),
    {127, 0, 0, 1} = leptus_config:ip_addr().

port_num(_) ->
    8080 = leptus_config:port_num(),

    leptus_config:set(http, [{port, 9000}]),
    9000 = leptus_config:port_num(),

    leptus_config:set(http, [{port, 4000}]),
    4000 = leptus_config:port_num(),

    leptus_config:set(http, [{port, undefined}]),
    8080 = leptus_config:port_num().

handlers(_) ->
    [] = leptus_config:handlers(),

    leptus_config:set(handlers, [{rh1, undefined_state}]),
    [{rh1, undefined_state}] = leptus_config:handlers(),

    leptus_config:set(handlers, [{rh1, undefined_state}, {rh2, nothing}]),
    [{rh1, undefined_state}, {rh2, nothing}] = leptus_config:handlers(),

    leptus_config:set(handlers, []),
    [] = leptus_config:handlers().
