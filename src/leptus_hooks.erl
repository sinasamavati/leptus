%% The MIT License

%% Copyright (c) 2013-2014 Sina Samavati <sina.samv@gmail.com>

%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:

%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.

%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.

-module(leptus_hooks).

-export([console_log/4]).


console_log(Status, Headers, _, Req) ->
    %% [%Y-%m-%d %H:%M:%S] "METHOD URL VERSION" STATUS CONTENT-LENGTH
    {{Year, Month, Day}, {Hour, Min, Sec}} = erlang:localtime(),
    Method = leptus_req:method(Req),
    Uri = leptus_req:uri(Req),
    Version = leptus_req:version(Req),
    ContentLength = get_value(<<"content-length">>, Headers, 0),
    io:format("[~w-~w-~w ~w:~w:~w] \"\~s ~s ~s\"\ ~w ~s~n",
              [Year, Month, Day, Hour, Min, Sec, Method, Uri,
               Version, Status, ContentLength]),
    Req.


%% internal
get_value(Key, Proplist, Default) ->
    case lists:keyfind(Key, 1, Proplist) of
        {_, Value} -> Value;
        _ -> Default
    end.
