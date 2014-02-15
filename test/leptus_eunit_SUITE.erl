-module(leptus_eunit_SUITE).

-export([all/0]).
-export([run_eunit/1]).

all() ->
    [run_eunit].

run_eunit(_) ->
    ok = eunit:test({application, leptus}).
