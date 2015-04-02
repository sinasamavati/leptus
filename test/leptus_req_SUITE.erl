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

-module(leptus_req_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([init_per_testcase/2]).
-export([end_per_testcase/2]).
-export([groups/0]).
-export([all/0]).
-export([param/1]).
-export([params/1]).
-export([qs/1]).
-export([qs_val/1]).
-export([uri/1]).
-export([version/1]).
-export([body/1]).
-export([body_raw/1]).
-export([body_qs/1]).
-export([header/1]).
-export([parse_header/1]).
-export([auth/1]).
-export([method/1]).
-export([get_req/1]).
-export([set_req/1]).

init_per_testcase(_, Config) ->
    {ok, Req1} = leptus_req:start(req1()),
    {ok, Req2} = leptus_req:start(req2()),
    {ok, Req3} = leptus_req:start(req3()),
    {ok, Req4} = leptus_req:start(req4()),
    {ok, Req5} = leptus_req:start(req5()),
    {ok, Req6} = leptus_req:start(req6()),
    {ok, Req7} = leptus_req:start(req7()),
    [{req1, Req1}, {req2, Req2}, {req3, Req3}, {req4, Req4}, {req5, Req5},
     {req6, Req6}, {req7, Req7}] ++ Config.

end_per_testcase(_, Config) ->
    ok = leptus_req:stop(?config(req1, Config)),
    ok = leptus_req:stop(?config(req2, Config)),
    ok = leptus_req:stop(?config(req3, Config)),
    ok = leptus_req:stop(?config(req4, Config)),
    ok = leptus_req:stop(?config(req5, Config)),
    ok = leptus_req:stop(?config(req6, Config)),
    ok = leptus_req:stop(?config(req7, Config)),
    Config.

groups() ->
    [{main, [parallel], [param,
                         params,
                         qs,
                         qs_val,
                         uri,
                         version,
                         body,
                         body_raw,
                         body_qs,
                         header,
                         parse_header,
                         auth,
                         method,
                         get_req,
                         set_req]}].

all() ->
    [{group, main}].

param(Config) ->
    Req1 = ?config(req1, Config),
    Req2 = ?config(req2, Config),
    true = undefined =:= leptus_req:param(Req1, namez),
    <<"leptus">> = leptus_req:param(Req1, name),
    true = undefined =:= leptus_req:param(Req1, idz),
    <<"97dba1">> = leptus_req:param(Req1, id),
    undefined = leptus_req:param(Req2, id),
    ok.

params(Config) ->
    Req1 = ?config(req1, Config),
    Req2 = ?config(req2, Config),
    [{name, <<"leptus">>}, {id, <<"97dba1">>}] = leptus_req:params(Req1),
    [] = leptus_req:params(Req2),
    ok.

qs(Config) ->
    Req1 = ?config(req1, Config),
    Req2 = ?config(req2, Config),
    <<>> = leptus_req:qs(Req1),
    <<"q=123&b=456">> = leptus_req:qs(Req2),
    ok.

qs_val(Config) ->
    Req1 = ?config(req1, Config),
    Req2 = ?config(req2, Config),
    undefined = leptus_req:qs_val(Req1, <<"p">>),
    undefined = leptus_req:qs_val(Req2, <<"p">>),
    <<"123">> = leptus_req:qs_val(Req2, <<"q">>),
    <<"456">> = leptus_req:qs_val(Req2, <<"b">>),
    ok.

uri(Config) ->
    Req1 = ?config(req1, Config),
    Req2 = ?config(req2, Config),
    <<"/hello/leptus/97dba1">> = leptus_req:uri(Req1),
    <<"/hello/leptus/97dba1?q=123&b=456">> = leptus_req:uri(Req2),
    ok.

version(Config) ->
    Req1 = ?config(req1, Config),
    Req2 = ?config(req2, Config),
    'HTTP/1.1' = leptus_req:version(Req1),
    'HTTP/1.1' = leptus_req:version(Req2),
    ok.

body(Config) ->
    Req1 = ?config(req1, Config),
    Req3 = ?config(req3, Config),
    Req5 = ?config(req5, Config),
    Req7 = ?config(req7, Config),

    <<>> = leptus_req:body(Req1),
    <<"AAAAAAAAAAA">> = leptus_req:body(Req3),

    %% when content-type is application/json
    [
     {<<"firstname">>, <<"Sina">>},
     {<<"lastname">>, <<"Samavati">>}
    ] = leptus_req:body(Req5),

    %% when content-type is application/x-msgpack
    [
     {<<"abc">>, 123},
     {456, <<"def">>}
    ] = leptus_req:body(Req7),
    ok.

body_raw(Config) ->
    Req1 = ?config(req1, Config),
    Req3 = ?config(req3, Config),
    Req5 = ?config(req5, Config),
    <<>> = leptus_req:body_raw(Req1),
    <<"AAAAAAAAAAA">> = leptus_req:body_raw(Req3),
    <<"{\"firstname\": \"Sina\","
      " \"lastname\": \"Samavati\"}">> = leptus_req:body_raw(Req5),
    ok.

body_qs(Config) ->
    Req1 = ?config(req1, Config),
    Req3 = ?config(req3, Config),
    Req4 = ?config(req4, Config),
    [] = leptus_req:body_qs(Req1),
    [{<<"AAAAAAAAAAA">>, true}] = leptus_req:body_qs(Req3),
    [
     {<<"firstname">>, <<"Sina">>},
     {<<"lastname">>, <<"Samavati">>}
    ] = leptus_req:body_qs(Req4),
    ok.

header(Config) ->
    Req1 = ?config(req1, Config),
    Req2 = ?config(req2, Config),
    Req3 = ?config(req3, Config),
    undefined = leptus_req:header(Req1, <<"content-type">>),
    <<"localhost:8080">> = leptus_req:header(Req2, <<"host">>),
    <<"application/x-www-form-urlencoded">> =
        leptus_req:header(Req3, <<"content-type">>),
    ok.

parse_header(Config) ->
    Req1 = ?config(req1, Config),
    Req2 = ?config(req2, Config),
    Req3 = ?config(req3, Config),
    Req5 = ?config(req5, Config),
    undefined = leptus_req:parse_header(Req1, <<"content-type">>),
    <<"localhost:8080">> = leptus_req:parse_header(Req2, <<"host">>),
    {
      <<"application">>, <<"x-www-form-urlencoded">>, []
    } = leptus_req:parse_header(Req3, <<"content-type">>),
    {
      <<"application">>, <<"json">>, []
    } = leptus_req:parse_header(Req5, <<"content-type">>),
    ok.

auth(Config) ->
    Req1 = ?config(req1, Config),
    Req3 = ?config(req3, Config),
    Req5 = ?config(req5, Config),
    Req6 = ?config(req6, Config),
    undefined = leptus_req:auth(Req1, basic),
    undefined = leptus_req:auth(Req3, basic),
    {<<"123">>, <<"456">>} = leptus_req:auth(Req5, basic),
    {error, badarg} = leptus_req:auth(Req6, basic),
    ok.

method(Config) ->
    Req1 = ?config(req1, Config),
    Req2 = ?config(req2, Config),
    Req3 = ?config(req3, Config),
    Req4 = ?config(req4, Config),
    <<"GET">> = leptus_req:method(Req1),
    <<"GET">> = leptus_req:method(Req2),
    <<"POST">> = leptus_req:method(Req3),
    <<"POST">> = leptus_req:method(Req4),
    ok.

get_req(Config) ->
    Req1 = ?config(req1, Config),
    true = (req1() =:= leptus_req:get_req(Req1)),
    ok.

set_req(Config) ->
    Req1 = ?config(req1, Config),
    ok = leptus_req:set_req(Req1, empty),
    empty = leptus_req:get_req(Req1),
    ok.


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

req5() ->
    {http_req,port,ranch_tcp,keepalive,pid,<<"POST">>,
     'HTTP/1.1',
     {{127,0,0,1},33977},
     <<"localhost">>,undefined,8080,<<"/">>,undefined,<<>>,undefined,
     [],[{<<"authorization">>,<<"Basic MTIzOjQ1Ng==">>},
         {<<"user-agent">>,
          <<"curl/7.22.0 (x86_64-pc-linux-gnu) libcurl/7.22.0 OpenSSL/1.0.1"
            "zlib/1.2.3.4 libidn/1.23 librtmp/2.3">>},
         {<<"host">>,<<"localhost:8080">>},
         {<<"accept">>,<<"*/*">>},
         {<<"content-type">>,<<"application/json">>},
         {<<"content-length">>,<<"45">>}],
     [],undefined,[],waiting,undefined,
     <<"{\"firstname\": \"Sina\", \"lastname\": \"Samavati\"}">>,
     false,waiting,[],<<>>,console_log}.

req6() ->
    {http_req,port,ranch_tcp,keepalive,pid,<<"POST">>,
     'HTTP/1.1',
     {{127,0,0,1},33977},
     <<"localhost">>,undefined,8080,<<"/">>,undefined,<<>>,undefined,
     [],[{<<"authorization">>,<<"Basic c2luYXdyb3RlX21lOg==">>},
         {<<"user-agent">>,
          <<"curl/7.22.0 (x86_64-pc-linux-gnu) libcurl/7.22.0 OpenSSL/1.0.1"
            "zlib/1.2.3.4 libidn/1.23 librtmp/2.3">>},
         {<<"host">>,<<"localhost:8080">>},
         {<<"accept">>,<<"*/*">>},
         {<<"content-type">>,<<"application/json">>},
         {<<"content-length">>,<<"45">>}],
     [],undefined,[],waiting,undefined,
     <<"{\"firstname\": \"Sina\", \"lastname\": \"Samavati\"}">>,
     false,waiting,[],<<>>,console_log}.

req7() ->
    {http_req,port,ranch_tcp,keepalive,pid,<<"POST">>,
     'HTTP/1.1',
     {{127,0,0,1},60773},
     <<"localhost">>,undefined,8080,<<"/msgpack">>,undefined,<<>>,
     undefined,[],
     [{<<"content-length">>,<<"13">>},
      {<<"host">>,<<"localhost:8080">>},
      {<<"user-agent">>,<<"Cow">>},
      {<<"content-type">>,<<"application/x-msgpack">>}],
     [],undefined,[],waiting,undefined,
     <<130,163,97,98,99,123,205,1,200,163,100,101,102>>,
     false,waiting,[],<<>>,console_log}.
