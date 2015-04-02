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

-module(leptus_handler_SUITE).

-export([all/0]).
-export([join_methods/1]).
-export([compile_host/1]).
-export([origin_matches/1]).

all() ->
    [join_methods, compile_host, origin_matches].

join_methods(_) ->
    <<"GET, PUT">> = leptus_handler:join_http_methods([<<"GET">>, <<"PUT">>]),
    <<"GET, PUT, POST, DELETE">> =
        leptus_handler:join_http_methods([<<"GET">>, <<"PUT">>, <<"POST">>,
                                          <<"DELETE">>]),
    ok.

compile_host(_) ->
    ['_'] = leptus_handler:compile_host('_'),
    [[<<"org">>, <<"example">>]] = leptus_handler:compile_host("example.org"),
    [[<<"org">>, <<"example">>], [<<"org">>, <<"example">>, <<"www">>]] =
        leptus_handler:compile_host(<<"[www.]example.org">>),
    ok.

origin_matches(_) ->
    O1 = <<"example.org">>,
    true = leptus_handler:origin_matches(O1, ['_']),
    true = leptus_handler:origin_matches(O1, [<<"example.org">>]),
    true = leptus_handler:origin_matches(O1, [<<"[...]example.org">>]),
    true = leptus_handler:origin_matches(O1, [<<"example.[...]">>]),
    true = leptus_handler:origin_matches(O1, [<<"www.example.org">>, <<"[www.]example.org">>]),
    true = leptus_handler:origin_matches(O1, ["whatever.:_", "[...]:_"]),
    false = leptus_handler:origin_matches(O1, [<<":subdomain.example.org">>]),
    false = leptus_handler:origin_matches(O1, ["[www.]example.com", "example.com"]),
    false = leptus_handler:origin_matches(O1, ["examplez[...]"]),

    O2 = <<"sub.example.org">>,
    true = leptus_handler:origin_matches(O2, ["sub.example.:_"]),
    true = leptus_handler:origin_matches(O2, ["asdf.com", "123.org", "x.io", "sub.example.org"]),
    true = leptus_handler:origin_matches(O2, ["[...]example.org"]),
    false = leptus_handler:origin_matches(O2, ["www.example.org", "[www.]example.org"]),
    false = leptus_handler:origin_matches(O2, ["whatever.:_"]),
    false = leptus_handler:origin_matches(O2, [<<"subdomain.:[...]">>]),

    true = leptus_handler:origin_matches(O1, ["[sub.]example.org"]),
    true = leptus_handler:origin_matches(O2, ["[sub.]example.org"]),
    ok.
