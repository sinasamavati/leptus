%% Copyright (c) 2013-2018 Sina Samavati <sina.samv@gmail.com>
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

-module(leptus).

%% -----------------------------------------------------------------------------
%% API
%% -----------------------------------------------------------------------------
-export([start_listener/2]).
-export([start_listener/3]).
-export([start_listener/4]).
-export([upgrade/0]).
-export([upgrade/1]).
-export([upgrade/2]).
-export([upgrade/3]).
-export([stop_listener/1]).
-export([running_listeners/0]).
-export([listener_uptime/1]).

-include("leptus_stats.hrl").

%% -----------------------------------------------------------------------------
%% types
%% -----------------------------------------------------------------------------
-type host_match() :: term().
-type handler() :: {module(), State :: any()}.
-type handlers() :: [{host_match(), [handler()]}].
-export_type([handler/0]).
-export_type([handlers/0]).

-type listener() :: http | https.
-type static_directory() :: Dir :: file:name()
                          | {priv_dir, App :: atom(), Dir :: file:name()}.
-type option() :: {num_acceptors, non_neg_integer()}
                | {ip, inet:ip_address()}
                | {port, inet:port_number()}
                | {cacertfile, file:name_all()}
                | {certfile, file:name_all()}
                | {keyfile, file:name_all()}
                | {static_dir, {host_match(), static_directory()}}
                | {log_handlers, [{module(), any()}]}.
-type options() :: [option()].
-export_type([listener/0]).
-export_type([options/0]).


%% -----------------------------------------------------------------------------
%% start a listener
%% -----------------------------------------------------------------------------
-spec start_listener(listener(), atom() | handlers()) ->
                            {ok, pid()} | {error, any()}.
start_listener(Listener, App) when is_atom(App)->
    %% App/priv/leptus.config should have two sections:
    %%   * {handlers, handlers()}
    %%   * {options, options()}
    Conf = leptus_config:config_file(App),
    Handlers = opt(handlers, Conf, []),
    Opts = opt(options, Conf, []),
    start_listener(Listener, Handlers, Opts);
start_listener(Listener, Handlers) ->
    start_listener(Listener, Handlers, []).

-spec start_listener(listener(), handlers(), options()) ->
                            {ok, pid()} | {error, any()}.
start_listener(Listener, Handlers, Opts) ->
    start_listener(Listener, Handlers, Opts, #{}).

-spec start_listener(listener(), handlers(), options(), cowboy_protocol:opts()) ->
                            {ok, pid()} | {error, any()}.
start_listener(Listener, Handlers, Opts, CowboyProtoOpts0) ->
    %% add log handlers to the event manager
    LogHandlers = opt(log_handlers, Opts, []),
    [ok = leptus_logger:add_handler(M, A) || {M, A} <- LogHandlers],

    %% routes
    Paths0 = leptus_router:paths(Handlers),
    Paths1 = case opt(static_dir, Opts, undefined) of
                 undefined ->
                     [];
                 Path ->
                     %% serving static files
                     leptus_router:static_file_routes(Path)
             end,
    Dispatch0 = cowboy_router:compile(Paths0 ++ Paths1),
    %% sort compiled routes
    Dispatch1 = leptus_router:sort_dispatch(Dispatch0),

    ListenerFunc = get_listener_func(Listener),
    Ref = get_ref(Listener),

    ListenerOpts = listener_opts(Listener, Opts),
    CowboyProtoOpts = maps:merge(
                        #{env => #{dispatch => Dispatch1}},
                        CowboyProtoOpts0
                       ),

    Res = cowboy:ListenerFunc(Ref, ListenerOpts, CowboyProtoOpts),
    case Res of
        {ok, _} ->
            update_listener_bucket({Listener, {Handlers, ListenerOpts}}),
            print_info(Listener, ListenerOpts);
        _ ->
            ok
    end,
    Res.

%% -----------------------------------------------------------------------------
%% upgrade running listeners
%% -----------------------------------------------------------------------------
-spec upgrade() -> ok.
upgrade() ->
    upgrade(running_listeners()).

-spec upgrade([listener()]) -> ok.
upgrade(Listeners) ->
    LH = [{L, leptus_utils:listener_handlers(L)} || L <- Listeners],
    [upgrade(L, H) || {L, H} <- LH],
    ok.

%% -----------------------------------------------------------------------------
%% upgrade a listener
%% -----------------------------------------------------------------------------
-spec upgrade(listener(), handlers()) -> ok.
upgrade(Listener, Handlers) ->
    Opts = case leptus_utils:listener_bucket(Listener) of
               not_found ->
                   [];
               #listener_bucket{options = Opts1} ->
                   Opts1
           end,
    upgrade(Listener, Handlers, Opts).

-spec upgrade(listener(), handlers(), options()) -> ok.
upgrade(Listener, Handlers, Opts) ->
    Paths = leptus_router:paths(Handlers),
    Paths1 = case opt(static_dir, Opts, undefined) of
                 undefined ->
                     [];
                 Path ->
                     leptus_router:static_file_routes(Path)
             end,
    Dispatch = cowboy_router:compile(Paths ++ Paths1),
    %% sort compiled routes
    Dispatch1 = leptus_router:sort_dispatch(Dispatch),
    Ref = get_ref(Listener),
    cowboy:set_env(Ref, dispatch, Dispatch1).

%% -----------------------------------------------------------------------------
%% stop a listener
%% -----------------------------------------------------------------------------
-spec stop_listener(listener()) -> ok | {error, not_found}.
stop_listener(Listener) ->
    cowboy:stop_listener(get_ref(Listener)).

%% -----------------------------------------------------------------------------
%% get a list of running listeners
%% -----------------------------------------------------------------------------
-spec running_listeners() -> [listener()].
running_listeners() ->
    F = fun({L, _}, Acc) ->
                [L|Acc]
        end,
    lists:foldr(F, [], leptus_config:lookup(listeners, [])).

%% -----------------------------------------------------------------------------
%% get uptime of a running listener
%% -----------------------------------------------------------------------------
-spec listener_uptime(listener()) -> {Days :: integer(), calendar:time()} |
                                     {error, not_found}.
listener_uptime(Listener) ->
    case leptus_utils:listener_bucket(Listener) of
        not_found ->
            {error, not_found};
        #listener_bucket{started_timestamp = Started} ->
            StartedDatetime = calendar:now_to_local_time(Started),
            Localtime = calendar:local_time(),
            calendar:time_difference(StartedDatetime, Localtime)
    end.

%% -----------------------------------------------------------------------------
%% internal
%% -----------------------------------------------------------------------------
-spec get_listener_func(listener()) -> atom().
get_listener_func(http) -> start_clear;
get_listener_func(https) -> start_tls.

-spec get_ref(listener()) -> ranch:ref().
get_ref(http) -> leptus_http;
get_ref(https) -> leptus_https.

%% -----------------------------------------------------------------------------
%% listener options
%% -----------------------------------------------------------------------------
-spec listener_opts(listener(), options()) -> options().
listener_opts(http, Opts) ->
    basic_listener_opts(Opts);
listener_opts(https, Opts) ->
    basic_listener_opts(Opts) ++ extra_listener_opts(Opts).

-spec basic_listener_opts(options()) -> options().
basic_listener_opts(Opts) ->
    [
     {ip, opt(ip, Opts, {127, 0, 0, 1})},
     {port, opt(port, Opts, 8080)},
     {num_acceptors, opt(num_acceptors, Opts, 100)}
    ].

-spec extra_listener_opts(options()) -> options().
extra_listener_opts(Opts) ->
    [
     {cacertfile, opt(cacertfile, Opts, "")},
     {certfile, opt(certfile, Opts, "")},
     {keyfile, opt(keyfile, Opts, "")}
    ].

-spec opt(atom(), options(), Default) -> any() | Default when Default :: any().
opt(Key, [{Key, Value}|_], _) ->
    Value;
opt(Key, [_|Rest], Default) ->
    opt(Key, Rest, Default);
opt(_, [], Default) ->
    Default.

%% -----------------------------------------------------------------------------
%% print the version number and what ip/port it's started on
%% -----------------------------------------------------------------------------
-spec print_info(listener(), options()) -> ok.
print_info(Listener0, Opts) ->
    {ok, Vsn} = application:get_key(leptus, vsn),
    Listener = case Listener0 of
                   http -> "http";
                   https -> "https"
               end,
    IP = opt(ip, Opts, undefined),
    Port = opt(port, Opts, undefined),
    io:format("Leptus ~s started on ~s://~s:~p~n",
              [Vsn, Listener, inet_parse:ntoa(IP), Port]).

%% -----------------------------------------------------------------------------
%% update leptus_config ETS table
%% keep handlers and options in an ETS table
%% -----------------------------------------------------------------------------
-spec update_listener_bucket({listener(), {handlers(), options()}}) -> ok.
update_listener_bucket({Listener, {Handlers, Opts}}) ->
    %% [{Listener, Bucket}]
    Bucket = #listener_bucket{handlers = Handlers, options = Opts,
                              started_timestamp = erlang:timestamp()},
    Listeners = leptus_config:lookup(listeners, []),
    Listeners1 = lists:keystore(Listener, 1, Listeners, {Listener, Bucket}),
    leptus_config:set(listeners, Listeners1).
