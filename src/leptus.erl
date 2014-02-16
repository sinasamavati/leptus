%% This file is part of leptus, and released under the MIT license.
%% See LICENSE for more information.

-module(leptus).
-author("Sina Samavati <sina.samv@gmail.com>").

-export([start_listener/2]).
-export([start_listener/3]).
-export([stop_listener/1]).

-type host_match() :: term().
-type handler() :: {module(), State :: any()}.
-type handlers() :: [{host_match(), [handler()]}].
-export_type([handler/0]).
-export_type([handlers/0]).

-type listener() :: http | https | spdy.
-type option() :: {ip, inet:ip_address()}
                | {port, inet:port_number()}
                | {cacertfile, file:name_all()}
                | {certfile, file:name_all()}
                | {keyfile, file:name_all()}.
-type options() :: [option()].
-export_type([listener/0]).
-export_type([options/0]).


-spec start_listener(listener(), handlers()) -> {ok, pid()} | {error, any()}.
start_listener(Listener, Handlers) ->
    start_listener(Listener, Handlers, []).

-spec start_listener(listener(), handlers(), options()) ->
                            {ok, pid()} | {error, any()}.
start_listener(Listener, Handlers, Opts) ->
    ensure_deps_started(),
    ensure_started(leptus),

    %% routes
    Paths = leptus_router:paths(Handlers),
    Dispatch = cowboy_router:compile(Paths),
    %% sort compiled routes
    Dispatch1 = leptus_router:sort_dispatch(Dispatch),

    %% basic listener configuration
    IP = {ip, get_value(ip, Opts, {127, 0, 0, 1})},
    Port = {port, get_value(port, Opts, 8080)},

    ListenerFunc = get_listener_func(Listener),
    Ref = get_ref(Listener),
    cowboy:ListenerFunc(Ref, 100,
                        [IP, Port] ++ get_extra_opts(Listener, Opts),
                        [
                         {env, [{dispatch, Dispatch1}]},
                         {onresponse, fun leptus_hooks:console_log/4}
                        ]).

-spec stop_listener(listener()) -> ok | {error, not_found}.
stop_listener(Listener) ->
    cowboy:stop_listener(get_ref(Listener)).

-spec get_listener_func(listener()) -> atom().
get_listener_func(http) -> start_http;
get_listener_func(https) -> start_https;
get_listener_func(spdy) -> start_spdy.

-spec get_ref(listener()) -> ranch:ref().
get_ref(http) -> leptus_http;
get_ref(https) -> leptus_https;
get_ref(spdy) -> leptus_spdy.

%% get extra options based on listener
-spec get_extra_opts(listener(), options()) -> [none() | options()].
get_extra_opts(http, _) -> [];
get_extra_opts(_, Opts) ->
    [
     {cacertfile, get_value(cacertfile, Opts, "")},
     {certfile, get_value(certfile, Opts, "")},
     {keyfile, get_value(keyfile, Opts, "")}
    ].

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.

%% ensure dependencies are started
ensure_deps_started() ->
    ensure_started(crypto),
    ensure_started(ranch),
    ensure_started(cowlib),
    ensure_started(cowboy).

get_value(_, [], Default) ->
    Default;
get_value(_, undefined, Default) ->
    Default;
get_value(Key, Opts, Default) ->
    case lists:keyfind(Key, 1, Opts) of
        {_, V} -> V;
        _ -> Default
    end.
