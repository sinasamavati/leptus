%% This file is part of leptus, and released under the MIT license.
%% See LICENSE for more information.

-module(leptus_config).
-behaviour(gen_server).

-author("Sina Samavati <sina.samv@gmail.com>").

%% gen_server
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-export([start/0]).
-export([start_link/0]).
-export([stop/0]).
-export([lookup/1]).
-export([lookup/2]).
-export([set/2]).
-export([config_file/1]).
-export([priv_dir/1]).


%% for test purposes
-spec start() -> {ok, pid()} | {error, any()}.
start() ->
    gen_server:start({local, ?MODULE}, ?MODULE, [], []).

-spec start_link() -> {ok, pid()} | {error, any()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec stop() -> ok.
stop() ->
    gen_server:cast(?MODULE, stop).

%% find a k/v
-spec lookup(any()) -> any() | undefined.
lookup(Key) ->
    lookup(Key, undefined).

%% find a k/v and return Default if not found
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

%% set a k/v
-spec set(any(), any()) -> ok.
set(Key, Value) ->
    gen_server:call(?MODULE, {set, {Key, Value}}).

%% read priv/leptus.config file
config_file(App) ->
    case file:consult(filename:join([priv_dir(App), "leptus.config"])) of
        {ok, Terms} ->
            Terms;
        Else ->
            Else
    end.

%% find the path to the priv directory in an application
priv_dir(App) ->
    case code:priv_dir(App) of
        {error, bad_name} ->
            Ebin = filename:dirname(code:which(App)),
            filename:join(filename:dirname(Ebin), "priv");
        PrivDir ->
            PrivDir
    end.


%% gen_server
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
