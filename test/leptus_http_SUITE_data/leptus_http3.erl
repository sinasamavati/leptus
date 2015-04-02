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

-module(leptus_http3).
-compile({parse_transform, leptus_pt}).

%% leptus callbacks
-export([init/3]).
-export([post/3]).
-export([put/3]).
-export([delete/3]).
-export([terminate/4]).


init(_Route, _Req, _State) ->
    {ok, my_state}.

post("/user/register", Req, State) ->
    Body = leptus_req:body_qs(Req),
    Username = proplists:get_value(<<"username">>, Body),
    case Username of
        <<"asdf">> ->
            {403, <<"Username is already taken.">>, State};
        _ ->
            {201, <<"Thanks for registration.">>, State}
    end.

put("/settings/change-password", Req, State) ->
    [
     {<<"password">>, P1}, {<<"password_confirmation">>, P2}
    ] = leptus_req:body_qs(Req),

    if P1 =:= P2 ->
            {<<"Your password has been changed.">>, State};
       true ->
            {403, <<"Passwords didn't match.">>, badmatch}
    end.

delete("/users/:username/posts/:id", Req, State) ->
    my_state = State,
    IdLen = byte_size(leptus_req:param(Req, id)),
    if IdLen >= 4 ->
            {404, <<>>, dammit};

       true ->
            {204, <<>>, aha}
    end.

terminate(_Reason, _Route, _Req, State) ->
    case State of
        my_state -> ok;
        badmatch -> ok;
        aha -> ok;
        dammit -> ok
    end.
