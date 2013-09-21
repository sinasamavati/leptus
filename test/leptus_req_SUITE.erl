-module(leptus_req_SUITE).

-export([all/0]).
-export([binding/1]).
-export([bindings/1]).
-export([qs/1]).


all() ->
    [binding, bindings, qs].

binding(_) ->
    true = undefined =:= leptus_req:binding(namez, req1()),
    <<"leptus">> = leptus_req:binding(name, req1()),
    true = undefined =:= leptus_req:binding(idz, req1()),
    <<"97dba1">> = leptus_req:binding(id, req1()),
    undefined = leptus_req:binding(id, req2()).

bindings(_) ->
    [{name, <<"leptus">>}, {id, <<"97dba1">>}] = leptus_req:bindings(req1()),
    [] = leptus_req:bindings(req2()).

qs(_) ->
    <<>> = leptus_req:qs(req1()),
    <<"q=123">> = leptus_req:qs(req2()).

req1() ->
    {http_req, port, ranch_tcp, keepalive, pid, <<"GET">>,
     'HTTP/1.1',
     {{127,0,0,1}, 34273},
     <<"localhost">>, undefined, 8080, <<"/hello/leptus/97dba1">>, undefined,
     <<>>, undefined,
     [{name, <<"leptus">>}, {id, <<"97dba1">>}],
     [{<<"user-agent">>,
       <<"curl/7.22.0 (x86_64-pc-linux-gnu) libcurl/7.22.0 OpenSSL/1.0.1"
         " zlib/1.2.3.4 libidn/1.23 librtmp/2.3">>},
      {<<"host">>, <<"localhost:8080">>},
      {<<"accept">>, <<"*/*">>}],
     [], undefined, [], waiting, undefined, <<>>, false, waiting, [], <<>>,
     undefined}.

req2() ->
    {http_req, port, ranch_tcp, keepalive, pid, <<"GET">>,
     'HTTP/1.1',
     {{127,0,0,1}, 34273},
     <<"localhost">>, undefined, 8080, <<"/hello/leptus/97dba1">>, undefined,
     <<"q=123">>, undefined, [],
     [{<<"user-agent">>,
       <<"curl/7.22.0 (x86_64-pc-linux-gnu) libcurl/7.22.0 OpenSSL/1.0.1"
         " zlib/1.2.3.4 libidn/1.23 librtmp/2.3">>},
      {<<"host">>, <<"localhost:8080">>},
      {<<"accept">>, <<"*/*">>}],
     [], undefined, [], waiting, undefined, <<>>, false, waiting, [], <<>>,
     undefined}.
