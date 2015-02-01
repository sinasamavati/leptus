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

-module(helpers).

-export([url/1]).
-export([request/2, request/3, request/4]).
-export([response_body/1]).


url(Route) when is_list(Route) ->
    R1 = list_to_binary(Route),
    <<"http://localhost:8080", R1/binary>>.

request(Method, Url) ->
    request(Method, Url, [], <<>>).

request(Method, Url, Headers) ->
    request(Method, Url, Headers, <<>>).

request(Method, Url, Headers, Body) ->
    {ok, Client} = cowboy_client:init([]),
    {ok, Client1} = cowboy_client:request(Method, url(Url), Headers, Body, Client),
    {ok, Status, Headers1, Client2} = cowboy_client:response(Client1),
    {Status, Headers1, Client2}.

response_body(Client) ->
    {ok, Data, _} = cowboy_client:response_body(Client),
    Data.
