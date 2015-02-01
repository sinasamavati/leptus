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

-module(leptus_config).
-behaviour(gen_server).

%% -----------------------------------------------------------------------------
%% gen_server
%% -----------------------------------------------------------------------------
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

%% -----------------------------------------------------------------------------
%% API
%% -----------------------------------------------------------------------------
-export([start/0]).
-export([start_link/0]).
-export([stop/0]).
-export([lookup/1]).
-export([lookup/2]).
-export([set/2]).
-export([config_file/1]).

%% -----------------------------------------------------------------------------
%% for test purposes
%% -----------------------------------------------------------------------------
-spec start() -> {ok, pid()} | {error, any()}.
start() ->
    gen_server:start({local, ?MODULE}, ?MODULE, [], []).

-spec start_link() -> {ok, pid()} | {error, any()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec stop() -> ok.
stop() ->
    gen_server:cast(?MODULE, stop).

%% -----------------------------------------------------------------------------
%% find a k/v
%% -----------------------------------------------------------------------------
-spec lookup(any()) -> any() | undefined.
lookup(Key) ->
    lookup(Key, undefined).

%% -----------------------------------------------------------------------------
%% find a k/v and return Default if not found
%% -----------------------------------------------------------------------------
-spec lookup(any(), Default) -> any() | Default.
lookup(Key, Default) ->
    case ets:lookup(?MODULE, Key) of
        [] ->
            Default;
        [{_, undefined}] ->
            Default;
        [{_, V}] ->
            V
    end.

%% -----------------------------------------------------------------------------
%% set a k/v
%% -----------------------------------------------------------------------------
-spec set(any(), any()) -> ok.
set(Key, Value) ->
    gen_server:call(?MODULE, {set, {Key, Value}}).

%% -----------------------------------------------------------------------------
%% read App/priv/leptus.config file
%% -----------------------------------------------------------------------------
config_file(App) ->
    case file:consult(filename:join(leptus_utils:priv_dir(App),
                                    "leptus.config")) of
        {ok, Terms} ->
            Terms;
        Else ->
            Else
    end.

%% -----------------------------------------------------------------------------
%% gen_server
%% -----------------------------------------------------------------------------
init([]) ->
    ets:new(?MODULE, [set, named_table, protected]),
    {ok, ?MODULE}.

handle_call({set, Arg}, _From, Tab) ->
    true = ets:insert(Tab, Arg),
    {reply, ok, Tab};
handle_call(_Msg, _From, Tab) ->
    {noreply, Tab}.

handle_cast(stop, Tab) ->
    {stop, normal, Tab};
handle_cast(_Msg, Tab) ->
    {noreply, Tab}.

handle_info(_Msg, Tab) ->
    {noreply, Tab}.

terminate(normal, _Tab) ->
    ok.

code_change(_OldVsn, Tab, _Extra) ->
    {ok, Tab}.
