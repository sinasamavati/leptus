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

-module(leptus_debug_log).
-behaviour(gen_event).

-include("leptus_logger.hrl").

%% -----------------------------------------------------------------------------
%% gen_event callbacks
%% -----------------------------------------------------------------------------
-export([init/1]).
-export([handle_event/2]).
-export([handle_call/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-define(LOG_FORMAT, "~h ~l ~u ~t \"~r\" ~s ~B \"~{referer}\" \"~{user-agent}\"").

init(default) ->
    {ok, ?LOG_FORMAT};
init(LogFmt) ->
    {ok, LogFmt}.

handle_event({debug_log, LogData}, LogFmt) ->
    Log = leptus_logger:format(LogFmt, LogData),
    console_log(LogData#log_data.status, Log),
    {ok, LogFmt};
handle_event(_, LogFmt) ->
    {ok, LogFmt}.

handle_call(_Request, LogFmt) ->
    {ok, ok, LogFmt}.

handle_info(_Info, LogFmt) ->
    {ok, LogFmt}.

terminate(_Args, _LogFmt) ->
    ok.

code_change(_OldVsn, LogFmt, _Extra) ->
    {ok, LogFmt}.

%% -----------------------------------------------------------------------------
%% print request date-time, requested URI, response status and content-length
%% -----------------------------------------------------------------------------
-spec console_log(leptus_logger:status_code(), string()) -> ok.
console_log(Status, FormatedLog) ->
    Color = status_color(Status),
    io:format("~s~s\e[0m~n", [Color, FormatedLog]).

%% -----------------------------------------------------------------------------
%% get terminal color escape code based on status code
%% -----------------------------------------------------------------------------
-spec status_color(non_neg_integer()) -> string().
status_color(N) when N >= 200, N < 300 -> "\e[32m"; %% green
status_color(N) when N >= 300, N < 400 -> "\e[34m"; %% blue
status_color(N) when N >= 400, N < 500 -> "\e[31m"; %% red
status_color(N) when N >= 500 -> "\e[1m\e[31m"; %% bold red
status_color(_) -> "".
