-module(leptus_req_SUITE).

-export([all/0]).
-export([binding/1]).
-export([bindings/1]).
-export([qs/1]).
-export([qs_val/1]).
-export([uri/1]).
-export([version/1]).
-export([body/1]).
-export([body_qs/1]).
-export([header/1]).


all() ->
    [binding, bindings, qs, qs_val, uri, version, body, body_qs, header].

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
    <<"q=123&b=456">> = leptus_req:qs(req2()).

qs_val(_) ->
    undefined = leptus_req:qs_val(<<"p">>, req1()),
    undefined = leptus_req:qs_val(<<"p">>, req2()),
    <<"123">> = leptus_req:qs_val(<<"q">>, req2()),
    <<"456">> = leptus_req:qs_val(<<"b">>, req2()).

uri(_) ->
    <<"/hello/leptus/97dba1">> = leptus_req:uri(req1()),
    <<"/hello/leptus/97dba1?q=123&b=456">> = leptus_req:uri(req2()).

version(_) ->
    'HTTP/1.1' = leptus_req:version(req1()),
    'HTTP/1.1' = leptus_req:version(req2()).

body(_) ->
    <<>> = leptus_req:body(req1()),
    <<"AAAAAAAAAAA">> = leptus_req:body(req3()).

body_qs(_) ->
    [] = leptus_req:body_qs(req1()),
    [{<<"AAAAAAAAAAA">>, true}] = leptus_req:body_qs(req3()),
    [
     {<<"firstname">>, <<"Sina">>},
     {<<"lastname">>, <<"Samavati">>}
    ] = leptus_req:body_qs(req4()).

header(_) ->
    <<>> = leptus_req:header(<<"content-type">>, req1()),
    <<"localhost:8080">> = leptus_req:header(<<"host">>, req2()),
    <<"application/x-www-form-urlencoded">> =
        leptus_req:header(<<"content-type">>, req3()).

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
     <<"q=123&b=456">>, undefined, [],
     [{<<"user-agent">>,
       <<"curl/7.22.0 (x86_64-pc-linux-gnu) libcurl/7.22.0 OpenSSL/1.0.1"
         " zlib/1.2.3.4 libidn/1.23 librtmp/2.3">>},
      {<<"host">>, <<"localhost:8080">>},
      {<<"accept">>, <<"*/*">>}],
     [], undefined, [], waiting, undefined, <<>>, false, waiting, [], <<>>,
     undefined}.

req3() ->
    {http_req, port, ranch_tcp, keepalive, pid, <<"POST">>,
     'HTTP/1.1',
     {{127,0,0,1},34372},
     <<"localhost">>, undefined, 8080, <<"/">>, undefined, <<>>, undefined,
     [], [{<<"user-agent">>,
           <<"curl/7.22.0 (x86_64-pc-linux-gnu) libcurl/7.22.0 OpenSSL/1.0.1"
             " zlib/1.2.3.4 libidn/1.23 librtmp/2.3">>},
          {<<"host">>, <<"localhost:8080">>},
          {<<"accept">>, <<"*/*">>},
          {<<"content-length">>, <<"11">>},
          {<<"content-type">>, <<"application/x-www-form-urlencoded">>}],
     [], undefined, [], waiting, undefined, <<"AAAAAAAAAAA">>, false,
     waiting, [], <<>>, undefined}.

req4() ->
    {http_req, port, ranch_tcp, keepalive, pid, <<"POST">>,
     'HTTP/1.1',
     {{127,0,0,1},34372},
     <<"localhost">>, undefined, 8080, <<"/">>, undefined, <<>>, undefined,
     [], [{<<"user-agent">>,
           <<"curl/7.22.0 (x86_64-pc-linux-gnu) libcurl/7.22.0 OpenSSL/1.0.1"
             " zlib/1.2.3.4 libidn/1.23 librtmp/2.3">>},
          {<<"host">>, <<"localhost:8080">>},
          {<<"accept">>, <<"*/*">>},
          {<<"content-length">>, <<"32">>},
          {<<"content-type">>, <<"application/x-www-form-urlencoded">>}],
     [], undefined, [], waiting, undefined,
     <<"firstname=Sina&lastname=Samavati">>, false, waiting, [], <<>>,
     undefined}.
