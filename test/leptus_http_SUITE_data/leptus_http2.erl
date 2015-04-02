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

-module(leptus_http2).
-compile({parse_transform, leptus_pt}).

%% leptus callbacks
-export([init/3]).
-export([is_authenticated/3]).
-export([get/3]).
-export([put/3]).
-export([post/3]).
-export([terminate/4]).


init(_Route, _Req, _State) ->
    {ok, blah}.

is_authenticated("/users/:id", Req, State) ->
    case leptus_req:method(Req) of
        <<"PUT">> ->
            check_auth(Req, State);
        <<"POST">> ->
            case check_auth(Req, State) of
                {false, _, _} ->
                    {false, {json, [{<<"error">>, <<"unauthorized">>}]}, State};
                Else ->
                    Else
            end;
        _ ->
            {true, State}
    end;
is_authenticated(_Route, _Req, State) ->
    {true, State}.

get("/users/:id", Req, State) ->
    Id = leptus_req:param(Req, id),
    {["aha, this is ", Id], State};

get("/users/:id/interests", Req, State) ->
    Id = leptus_req:param(Req, id),
    case Id of
        <<"s1n4">> ->
            {200, <<"Erlang and a lotta things else">>, State};
        <<"456">> ->
            {"art, photography...", State};
        _ ->
            {404, <<"not found...">>, State}
    end;

get("/users/:id/profile", Req, State) ->
    Body = [
            {<<"id">>, leptus_req:param(Req, id)},
            {<<"bio">>, <<"Erlanger">>},
            {<<"github">>, leptus_req:param(Req, id)}
           ],
    {200, {json, Body}, State}.

put("/users/:id", _Req, State) ->
    {200, <<"updated">>, State}.

post("/users/:id", _Req, State) ->
    {200, <<"updated">>, State}.


%% internal
check_auth(Req, State) ->
    case leptus_req:auth(Req, basic) of
        {<<"sina">>, <<"wrote_me">>} ->
            {true, State};
        _ ->
            {false, <<"unauthorized.">>, State}
    end.

terminate(_Reason, _Route, _Req, State) ->
    blah = State,
    ok.
