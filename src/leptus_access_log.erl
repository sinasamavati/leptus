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

-module(leptus_access_log).
-behaviour(gen_event).

%% -----------------------------------------------------------------------------
%% gen_event callbacks
%% -----------------------------------------------------------------------------
-export([init/1]).
-export([handle_event/2]).
-export([handle_call/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-define(FILENAME_SUFFIX, "-access.log").
-define(LOG_FORMAT, "~h ~l ~u ~t \"~r\" ~s ~B \"~{referer}\" \"~{user-agent}\"").

-record(state, {dir, io_dev, log_fmt, tref}).

init({Dir, LogFmt}) ->
    %% ask for a new file once a day
    {ok, TRef} = timer:send_interval(millsec_til_tomorrow(), open_new_file),
    {ok, IoDev} = open_file(Dir),
    {ok, #state{dir = Dir, io_dev = IoDev, log_fmt = LogFmt, tref = TRef}};
init(Dir) ->
    init({Dir, ?LOG_FORMAT}).

handle_event({access_log, LogData}, State=#state{io_dev=IoDev, log_fmt=LogFmt}) ->
    Log = leptus_logger:format(LogFmt, LogData),
    file:write(IoDev, [Log, $\n]),
    {ok, State};
handle_event(_, State) ->
    {ok, State}.

handle_call(_Request, State) ->
    {ok, ok, State}.

handle_info(open_new_file, State=#state{dir=Dir, io_dev=IoDev}) ->
    file:close(IoDev),
    {ok, IoDev1} = open_file(Dir),
    {ok, State#state{io_dev = IoDev1}};
handle_info(_Info, State) ->
    {ok, State}.

terminate(_Args, #state{io_dev=IoDev, tref=TRef}) ->
    file:close(IoDev),
    timer:cancel(TRef),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% -----------------------------------------------------------------------------
%% make filename. e.g. "Dir/2014_06_22-access.log"
%% -----------------------------------------------------------------------------
filename(Dir) ->
    {{Y, M, D}, _} = erlang:localtime(),
    filename:join(Dir, io_lib:format("~w_~2..0w_~2..0w~s",
                                     [Y, M, D, ?FILENAME_SUFFIX])).

%% -----------------------------------------------------------------------------
%% take directory, make a filename, and open file
%% -----------------------------------------------------------------------------
open_file(Dir) ->
    Filename = filename(Dir),
    filelib:ensure_dir(Filename),
    file:open(Filename, [append, raw]).

%% -----------------------------------------------------------------------------
%% milliseconds left until a new day arrives
%% -----------------------------------------------------------------------------
millsec_til_tomorrow() ->
    {_, {CH, CM, CS}} = erlang:localtime(),
    H = (23 - CH) * (60 * 60) * 1000,
    M = (59 - CM) * (60 * 1000),
    S = (59 - CS) * 1000,
    (H + M) + S.
