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
-export([ip_addr/1]).
-export([port_num/1]).
-export([handlers/0]).
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

%% get IP address to bind to
-spec ip_addr(atom()) -> inet:ip_address().
ip_addr(Section) ->
    Default = {127, 0, 0, 1},
    get_value(ip, lookup(Section), Default).


%% get http port to listen on
-spec port_num(atom()) -> inet:port_number().
port_num(Section) ->
    Default = 8080,
    get_value(port, lookup(Section), Default).

%% get handlers
-spec handlers() -> leptus:handlers() | [].
handlers() ->
    Default = [],
    lookup(handlers, Default).

%% read priv/leptus.config file
config_file(App) ->
    case file:consult(filename:join([priv_dir(App), "leptus.config"])) of
        {error, _} ->
            [];
        {ok, Terms} ->
            Terms
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

terminate(normal, _TabId) ->
    ok.

code_change(_OldVsn, TabId, _Extra) ->
    {ok, TabId}.


get_value(_, undefined, Default) ->
    Default;
get_value(_, [], Default) ->
    Default;
get_value(Key, Proplist, Default) ->
    case lists:keyfind(Key, 1, Proplist) of
        {_, undefined} -> Default;
        {_, Value} -> Value;
        _ -> Default
    end.
