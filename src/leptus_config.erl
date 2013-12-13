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

-export([start_link/0]).
-export([stop/0]).
-export([lookup/1]).
-export([lookup/2]).
-export([set/2]).
-export([ip_addr/0]).
-export([port_num/0]).
-export([handlers/0]).


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

%% get IP address to bind to
ip_addr() ->
    Default = {127, 0, 0, 1},
    get_value(ip, lookup(http), Default).


%% get http port to listen on
-spec port_num() -> non_neg_integer() | 8080.
port_num() ->
    Default = 8080,
    get_value(port, lookup(http), Default).

%% get handlers
-spec handlers() -> leptus:handlers() | [].
handlers() ->
    Default = [],
    lookup(handlers, Default).


%% gen_server
init([]) ->
    ets:new(?MODULE, [set, named_table, protected]),
    %% read leptus.config and insert configurations to ets
    Conf = config_file(),
    Http = get_value(http, Conf),
    Handlers = get_value(handlers, Conf),
    ets:insert(?MODULE, {http, Http}),
    ets:insert(?MODULE, {handlers, Handlers}),
    {ok, ?MODULE}.

handle_call({set, Arg}, _From, TabId) ->
    true = ets:insert(TabId, Arg),
    {reply, ok, TabId};
handle_call(_Msg, _From, TabId) ->
    {noreply, TabId}.

handle_cast(stop, TabId) ->
    {stop, normal, TabId};
handle_cast(_Msg, TabId) ->
    {noreply, TabId}.

handle_info(_Msg, TabId) ->
    {noreply, TabId}.

terminate(_Reason, _TabId) ->
    ok.

code_change(_OldVsn, TabId, _Extra) ->
    {ok, TabId}.


%% read priv/leptus.config file
config_file() ->
    {ok, Cwd} = file:get_cwd(),
    case file:consult(filename:join([Cwd, "priv", "leptus.config"])) of
        {error, _} ->
            [];
        {ok, Terms} ->
            Terms
    end.

get_value(Key, Proplist) ->
    get_value(Key, Proplist, undefined).

get_value(Key, Proplist, Default) ->
    case lists:keyfind(Key, 1, Proplist) of
        {_, undefined} -> Default;
        {_, Value} -> Value;
        _ -> Default
    end.
