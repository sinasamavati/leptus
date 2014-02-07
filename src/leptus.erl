-module(leptus).
-author("Sina Samavati <sina.samv@gmail.com>").

-export([start_http/1]).
-export([start_https/1]).
-export([start_spdy/1]).
-export([stop_http/0]).
-export([stop_https/0]).
-export([stop_spdy/0]).
-export([upgrade/0]).
-export([upgrade/1]).

-type handler() :: {module(), State :: any()}.
-type handlers() :: [handler()].
-export_type([handlers/0]).

-type listener() :: http | https | spdy.
-type listener_option() :: {ip, inet:ip_address()}
                         | {port, inet:port_number()}
                         | {hostmatch, cowboy_router:dispatch_match()}
                         | {cacertfile, file:name_all()}
                         | {certfile, file:name_all()}
                         | {keyfile, file:name_all()}.
-type option() :: {handlers, handlers()} | {listener(), [listener_option()]}.
-type options() :: [option()].
-export_type([options/0]).

-type app_name() :: atom().


-spec start_http(options() | app_name()) -> {ok, pid()} | {error, any()}.
start_http(OptionsOrApp) ->
    start_listener(http, OptionsOrApp).

-spec start_https(options() | app_name()) -> {ok, pid()} | {error, any()}.
start_https(OptionsOrApp) ->
    start_listener(https, OptionsOrApp).

-spec start_spdy(options() | app_name()) -> {ok, pid()} | {error, any()}.
start_spdy(OptionsOrApp) ->
    start_listener(spdy, OptionsOrApp).

-spec stop_http() -> ok | {error, not_found}.
stop_http() ->
    cowboy:stop_listener(leptus_http).

-spec stop_https() -> ok | {error, not_found}.
stop_https() ->
    cowboy:stop_listener(leptus_https).

-spec stop_spdy() -> ok | {error, not_found}.
stop_spdy() ->
    cowboy:stop_listener(leptus_spdy).

-spec upgrade() -> ok.
upgrade() ->
    upgrade(leptus_config:lookup(handlers)).

-spec upgrade(handlers()) -> ok.
upgrade(Handlers) ->
    leptus_config:set(handlers, Handlers),
    Handlers = leptus_config:lookup(handlers), %% make sure it's been set
    Listener = leptus_config:lookup(listener),
    Paths = leptus_router:paths(Handlers),
    HostMatch = get_value(hostmatch, leptus_config:lookup(Listener), '_'),
    Dispatch = cowboy_router:compile([{HostMatch, Paths}]),
    Ref = get_ref_name(Listener),
    cowboy:set_env(Ref, dispatch, Dispatch).


%% internal
start_listener(Listener, App) when is_atom(App) ->
    Options = leptus_config:config_file(App),
    Ref = get_ref_name(Listener),
    start_listener(Ref, Listener, Options);
start_listener(Listener, Options) when is_list(Options) ->
    Ref = get_ref_name(Listener),
    start_listener(Ref, Listener, Options);
start_listener(_, _) ->
    error(badarg).

start_listener(Ref, Listener, Options) ->
    ensure_deps_started(),
    ensure_started(leptus),

    Handlers = get_value(handlers, Options, []),
    ListenerOpts = get_value(Listener, Options, []),
    %% initialize 'listener', 'handlers' and Listener k/v
    leptus_config:set(listener, Listener),
    leptus_config:set(Listener, ListenerOpts),
    leptus_config:set(handlers, Handlers),

    %% routes
    Paths = leptus_router:paths(Handlers),
    HostMatch = get_value(hostmatch, ListenerOpts, '_'),
    Dispatch = cowboy_router:compile([{HostMatch, Paths}]),
    %% sort compiled routes
    Dispatch1 = leptus_router:sort_dispatch(Dispatch),

    %% basic listener configuration
    IP = get_value(ip, ListenerOpts, {127, 0, 0, 1}),
    Port = get_value(port, ListenerOpts, 8080),

    ListenerFunc = get_listener_func(Listener),
    cowboy:ListenerFunc(Ref, 100,
                        [{ip, IP}, {port, Port}] ++ get_extra_opts(ListenerOpts),
                        [
                         {env, [{dispatch, Dispatch1}]},
                         {onresponse, fun leptus_hooks:console_log/4}
                        ]).

get_listener_func(http) -> start_http;
get_listener_func(https) -> start_https;
get_listener_func(spdy) -> start_spdy.

get_ref_name(http) -> leptus_http;
get_ref_name(https) -> leptus_https;
get_ref_name(spdy) -> leptus_spdy.

%% get extra options based on listener
get_extra_opts(http) -> [];
get_extra_opts(Listener) ->
    Opts = leptus_config:lookup(Listener),
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
