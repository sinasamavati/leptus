%% This file is part of leptus, and released under the MIT license.
%% See LICENSE for more information.

%% a bunch of functions to deal with a request
-module(leptus_req).

-export([param/2]).
-export([params/1]).
-export([qs/1]).
-export([qs_val/2]).
-export([uri/1]).
-export([version/1]).
-export([method/1]).
-export([body/1]).
-export([body_raw/1]).
-export([body_qs/1]).
-export([header/2]).
-export([parse_header/2]).
-export([auth/2]).


-spec param(atom(), cowboy_req:req()) -> binary() | undefined.
param(Key, Req) ->
    invoke(binding, [Key, Req]).

-spec params(cowboy_req:req()) -> [{atom(), binary()}] | undefined.
params(Req) ->
    invoke(bindings, [Req]).

-spec qs(cowboy_req:req()) -> binary().
qs(Req) ->
    invoke(qs, [Req]).

-spec qs_val(binary(), cowboy_req:req()) -> binary() | undefined.
qs_val(Key, Req) ->
    invoke(qs_val, [Key, Req]).

-spec uri(cowboy_req:req()) -> binary().
uri(Req) ->
    Path = invoke(path, [Req]),
    QS = invoke(qs, [Req]),

    %% e.g <<"/path?query=string">>

    case QS of
        <<>> -> Path;
        _ -> <<Path/binary, "?", QS/binary>>
    end.

-spec version(cowboy_req:req()) -> cowboy:http_version().
version(Req) ->
    invoke(version, [Req]).

-spec method(cowboy_req:req()) -> binary().
method(Req) ->
    invoke(method, [Req]).

-spec body(cowboy_req:req()) -> binary() | leptus_handler:json_term() |
                                msgpack:msgpack_term() | {error, any()}.
body(Req) ->
    Body = body_raw(Req),
    case header(<<"content-type">>, Req) of
        %% decode body if content-type is json or msgpack
        <<"application/json">> ->
            leptus_json:decode(Body);
        <<"application/x-msgpack">> ->
            case msgpack:unpack(Body) of
                {ok, {UnpackedBody}} ->
                    UnpackedBody;
                Else ->
                    Else
            end;
        _ ->
            Body
    end.

-spec body_raw(cowboy_req:req()) -> binary().
body_raw(Req) ->
    invoke(body, [infinity, Req]).

-spec body_qs(cowboy_req:req()) -> [{binary(), binary() | true}].
body_qs(Req) ->
    invoke(body_qs, [infinity, Req]).

-spec header(binary(), cowboy_req:req()) -> binary().
header(Name, Req) ->
    invoke(header, [Name, Req, <<>>]).

-spec parse_header(binary(), cowboy_req:req()) -> any() | <<>>.
parse_header(Name, Req) ->
    invoke(parse_header, [Name, Req, <<>>]).

-spec auth(basic, cowboy_req:req()) -> {binary(), binary()} | <<>> | error.
auth(basic, Req) ->
    case parse_header(<<"authorization">>, Req) of
        {<<"basic">>, UserPass} ->
            UserPass;
        Value ->
            Value
    end.


%% internal
invoke(F, A) ->
    get_value(apply(cowboy_req, F, A)).

get_value({Value, _}) ->
    Value;
get_value({ok, Value, _}) ->
    Value;
get_value({undefined, Value, _}) ->
    Value.


%% tests
-ifdef(TEST).

param_test() ->
    true = undefined =:= leptus_req:param(namez, req1()),
    <<"leptus">> = leptus_req:param(name, req1()),
    true = undefined =:= leptus_req:param(idz, req1()),
    <<"97dba1">> = leptus_req:param(id, req1()),
    undefined = leptus_req:param(id, req2()).

params_test() ->
    [{name, <<"leptus">>}, {id, <<"97dba1">>}] = leptus_req:params(req1()),
    [] = leptus_req:params(req2()).

qs_test() ->
    <<>> = leptus_req:qs(req1()),
    <<"q=123&b=456">> = leptus_req:qs(req2()).

qs_val_test() ->
    undefined = leptus_req:qs_val(<<"p">>, req1()),
    undefined = leptus_req:qs_val(<<"p">>, req2()),
    <<"123">> = leptus_req:qs_val(<<"q">>, req2()),
    <<"456">> = leptus_req:qs_val(<<"b">>, req2()).

uri_test() ->
    <<"/hello/leptus/97dba1">> = leptus_req:uri(req1()),
    <<"/hello/leptus/97dba1?q=123&b=456">> = leptus_req:uri(req2()).

version_test() ->
    'HTTP/1.1' = leptus_req:version(req1()),
    'HTTP/1.1' = leptus_req:version(req2()).

body_test() ->
    <<>> = leptus_req:body(req1()),
    <<"AAAAAAAAAAA">> = leptus_req:body(req3()),

    %% when content-type is application/json
    [
     {<<"firstname">>, <<"Sina">>},
     {<<"lastname">>, <<"Samavati">>}
    ] = leptus_req:body(req5()),

    %% when content-type is application/x-msgpack
    [
     {<<"abc">>, 123},
     {456, <<"def">>}
    ] = leptus_req:body(req7()).

body_raw_test() ->
    <<>> = leptus_req:body_raw(req1()),
    <<"AAAAAAAAAAA">> = leptus_req:body_raw(req3()),
    <<"{\"firstname\": \"Sina\","
      " \"lastname\": \"Samavati\"}">> = leptus_req:body_raw(req5()).

body_qs_test() ->
    [] = leptus_req:body_qs(req1()),
    [{<<"AAAAAAAAAAA">>, true}] = leptus_req:body_qs(req3()),
    [
     {<<"firstname">>, <<"Sina">>},
     {<<"lastname">>, <<"Samavati">>}
    ] = leptus_req:body_qs(req4()).

header_test() ->
    <<>> = leptus_req:header(<<"content-type">>, req1()),
    <<"localhost:8080">> = leptus_req:header(<<"host">>, req2()),
    <<"application/x-www-form-urlencoded">> =
        leptus_req:header(<<"content-type">>, req3()).

parse_header_test() ->
    <<>> = leptus_req:parse_header(<<"content-type">>, req1()),
    <<"localhost:8080">> = leptus_req:parse_header(<<"host">>, req2()),
    {
      <<"application">>, <<"x-www-form-urlencoded">>, []
    } = leptus_req:parse_header(<<"content-type">>, req3()),
    {
      <<"application">>, <<"json">>, []
    } = leptus_req:parse_header(<<"content-type">>, req5()).

auth_test() ->
    <<>> = leptus_req:auth(basic, req1()),
    <<>> = leptus_req:auth(basic, req3()),
    {<<"123">>, <<"456">>} = leptus_req:auth(basic, req5()),
    error = leptus_req:auth(basic, req6()).

method_test() ->
    <<"GET">> = leptus_req:method(req1()),
    <<"GET">> = leptus_req:method(req2()),
    <<"POST">> = leptus_req:method(req3()),
    <<"POST">> = leptus_req:method(req4()).


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

-endif.
