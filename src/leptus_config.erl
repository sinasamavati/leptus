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
-export([get/1]).
-export([get/2]).
-export([set/2]).
-export([set/3]).
-export([ip_addr/0]).
-export([port_num/0]).
-export([handlers/0]).


-spec start_link() -> {ok, pid()} | {error, any()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec stop() -> ok.
stop() ->
    gen_server:cast(?MODULE, stop).

-spec get(any()) -> any() | undefined.
get(Key) ->
    get(undefined, Key).

-spec get(any(), any()) -> any() | undefined.
get(Section, Key) ->
    get(Section, Key, undefined).

-spec get(any(), any(), Default) -> any() | Default.
get(Section, Key, Default) ->
    case ets:match_object(?MODULE, {Section, Key, '_'}) of
        [] ->
            Default;
        [{_, _, undefined}] ->
            Default;
        [{_, _, V}] ->
            V
    end.

-spec set(any(), any()) -> ok.
set(Key, Value) ->
    set(undefined, Key, Value).

-spec set(any(), any(), any()) -> ok.
set(Section, Key, Value) ->
    gen_server:call(?MODULE, {set, {Section, Key, Value}}).

%% get IP address to bind to
ip_addr() ->
    Default = {127, 0, 0, 1},
    case get(http, ip) of
        undefined ->
            Default;
        Else ->
            case inet_parse:address(Else) of
                {ok, IP} ->
                    IP;
                {error, _} ->
                    Default
            end
    end.

%% get http port to listen on
-spec port_num() -> non_neg_integer() | 8080.
port_num() ->
    get(http, port, 8080).

%% get handlers
-spec handlers() -> leptus:handlers() | [].
handlers() ->
    get(undefined, handlers, []).

%% gen_server
init([]) ->
    ets:new(?MODULE, [set, named_table, protected, {keypos, 2}]),
    {ok, ?MODULE}.

handle_call({set, Arg}, _From, TabId) ->
    true = ets:insert(?MODULE, Arg),
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
