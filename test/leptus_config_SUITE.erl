-module(leptus_config_SUITE).

-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([all/0]).
-export([set/1]).
-export([get/1]).


init_per_suite(Config) ->
    {ok, P} = leptus_config:start_link(),
    unlink(P),
    Config.

end_per_suite(_Config) ->
    ok = leptus_config:stop().

all() ->
    [set, get].

set(_) ->
    ok = leptus_config:set(t1, <<1,2,3>>),
    ok = leptus_config:set(t2, <<"1,2,3">>),
    ok = leptus_config:set(http, port_num, 8080),
    ok = leptus_config:set(http, ip_addr, "127.0.0.1"),
    ok = leptus_config:set(http, [{port_num, 8080}, {ip_addr, "127.0.0.1"}]).

get(_) ->
    <<1,2,3>> = leptus_config:get(t1),
    <<"1,2,3">> = leptus_config:get(t2),
    8080 = leptus_config:get(http, port_num),
    "127.0.0.1" = leptus_config:get(http, ip_addr),
    [{port_num, 8080}, {ip_addr, "127.0.0.1"}] = leptus_config:get(http).
