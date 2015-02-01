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

-module(pt3).
-compile({parse_transform, leptus_pt}).

-export([start/0]).
-export([stop/0]).

%% leptus callbacks
-export([init/3]).
-export([get/3]).
-export([put/3]).
-export([post/3]).
-export([delete/3]).
-export([terminate/4]).

start() ->
    leptus:start_listener(http, [{'_', [{?MODULE, state0}]}]).

stop() ->
    leptus:stop_listener(http).

init(_Route, _Req, State) ->
    State = state0,
    {ok, state1}.

get("/", _, State) ->
    State = state1,
    {<<>>, state2}.

put("/old", _, State) ->
    State = state1,
    {200, <<>>, state3}.

post("/new", _, State) ->
    State = state1,
    {201, <<>>, state4}.

delete("/old", _, State) ->
    State = state1,
    {204, <<>>, State}.

terminate(_Reason, _Route, _Req, State) ->
    case State of
        state1 -> ok;
        state2 -> ok;
        state3 -> ok;
        state4 -> ok
    end.
