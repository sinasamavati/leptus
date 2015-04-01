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

-module(leptus_utils_SUITE).

-export([all/0]).
-export([get_uri_authority/1]).

all() ->
    [get_uri_authority].

get_uri_authority(_Config) ->
    <<"example.com">> =
        leptus_utils:get_uri_authority(<<"example.com">>),

    <<"example.com">> =
        leptus_utils:get_uri_authority(<<"example.com/">>),

    <<"example.com">> =
        leptus_utils:get_uri_authority(<<"http://example.com">>),

    <<"example.com">> =
        leptus_utils:get_uri_authority(<<"http://example.com/a/b/">>),

    <<"example.com">> =
        leptus_utils:get_uri_authority(<<"example.com/a/b/">>),

    <<"example.com:123">> =
        leptus_utils:get_uri_authority(<<"example.com:123/a/b/">>),

    <<"u:p@example.com:123">> =
        leptus_utils:get_uri_authority(<<"u:p@example.com:123/a/b/">>),

    <<"example">> =
        leptus_utils:get_uri_authority(<<"scheme://example">>),

    <<"example">> =
        leptus_utils:get_uri_authority(<<"scheme://example/">>),

    <<"example">> =
        leptus_utils:get_uri_authority(<<"://example/">>),

    <<"example:123">> =
        leptus_utils:get_uri_authority(<<"scheme://example:123/a/b/">>),
    ok.
