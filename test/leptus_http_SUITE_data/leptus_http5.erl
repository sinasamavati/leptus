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

-module(leptus_http5).
-compile({parse_transform, leptus_pt}).

-export([init/3]).
-export([is_authenticated/3]).
-export([has_permission/3]).
-export([get/3]).
-export([terminate/4]).

init(_Route, _Req, State) ->
    {ok, State}.

is_authenticated(_Route, Req, State) ->
    case leptus_req:auth(Req, basic) of
        {<<"123">>, <<"456">>} ->
            {true, no_perm};
        {<<"asdf">>, <<"zxcv">>} ->
            {true, State};
        _ ->
            {false, <<"unauthorized">>, State}
    end.

has_permission(_Route, _Req, State=no_perm) ->
    {false, <<"you don't have permission to do this shit!">>, State};
has_permission(_Route, _Req, State) ->
    {true, State}.

get("/needs-perm", _Req, State) ->
    {<<"hah, see you got permission">>, State}.

terminate(_Reason, _Route, _Req, _State) ->
    ok.
