%% @author Sina Samavati <sina.samv@gmail.com>

-module(leptus).
-author("Sina Samavati <sina.samv@gmail.com>").

-export([start_http/1]).
-export([start_http/2]).
-export([start_https/1]).
-export([start_https/2]).
-export([start_spdy/1]).
-export([start_spdy/2]).
-export([stop_http/0]).
-export([stop_https/0]).
-export([stop_spdy/0]).
-export([upgrade/0]).
-export([upgrade/1]).

-type handler() :: {module(), State::any()}.
-type handlers() :: [handler()].
-export_type([handlers/0]).

-type option() :: {priv_dir, atom()}.
-type options() :: [option()].
-export_type([options/0]).


-spec start_http(handlers()) -> {ok, pid()} | {error, any()}.
start_http(Handlers) ->
    start_listener(http, Handlers, []).

-spec start_http(handlers(), options()) -> {ok, pid()} | {error, any()}.
start_http(Handlers, Options) ->
    start_listener(http, Handlers, Options).

-spec start_https(handlers()) -> {ok, pid()} | {error, any()}.
start_https(Handlers) ->
    start_listener(https, Handlers, []).

-spec start_https(handlers(), options()) -> {ok, pid()} | {error, any()}.
start_https(Handlers, Options) ->
    start_listener(https, Handlers, Options).

-spec start_spdy(handlers()) -> {ok, pid()} | {error, any()}.
start_spdy(Handlers) ->
    start_listener(spdy, Handlers, []).

-spec start_spdy(handlers(), options()) -> {ok, pid()} | {error, any()}.
start_spdy(Handlers, Options) ->
    start_listener(spdy, Handlers, Options).

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
    upgrade(leptus_config:handlers()).

-spec upgrade(handlers()) -> ok.
upgrade(Handlers) ->
    Paths = leptus_router:paths(Handlers),
    cowboy:set_env(leptus_http, dispatch, cowboy_router:compile([{'_', Paths}])).


%% internal
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
    ensure_started(cowboy).

get_value(Key, Opts) ->
    get_value(Key, Opts, undefined).

get_value(Key, Opts, Default) ->
    case lists:keyfind(Key, 1, Opts) of
        {_, V} -> V;
        _ -> Default
    end.

start_listener(Listener, Handlers, Options) ->
    ensure_deps_started(),
    ensure_started(leptus),

    leptus_config:set(handlers, Handlers),
    case get_value(priv_dir, Options) of
        undefined ->
            ok;
        App ->
            %% set priv_dir
            leptus_config:set(priv_dir, App),
            %% read leptus.config and insert configurations to ets
            Conf = leptus_config:config_file(App),
            ListenerOpts = get_value(Listener, Conf),
            Handlers1 = get_value(handlers, Conf, Handlers),
            %% initialize 'handlers' and Listener k/v
            leptus_config:set(Listener, ListenerOpts),
            leptus_config:set(handlers, Handlers1)
    end,

    %% routes
    Paths = leptus_router:paths(leptus_config:handlers()),
    Dispatch = cowboy_router:compile([{'_', Paths}]),

    %% basic http configuration
    IP = leptus_config:ip_addr(),
    Port = leptus_config:port_num(),

    ListenerFunc = get_listener_func(Listener),
    Ref = get_ref_name(Listener),
    ExtraOpts = get_extra_opts(Listener, leptus_config:lookup(priv_dir)),
    cowboy:ListenerFunc(Ref, 100,
                        [{ip, IP}, {port, Port}] ++ ExtraOpts,
                        [
                         {env, [{dispatch, Dispatch}]},
                         {onresponse, fun leptus_hooks:console_log/4}
                        ]).

get_listener_func(http) -> start_http;
get_listener_func(https) -> start_https;
get_listener_func(spdy) -> start_spdy.

get_ref_name(http) -> leptus_http;
get_ref_name(https) -> leptus_https;
get_ref_name(spdy) -> leptus_spdy.

%% get extra optionns based on listener and priv_dir app
get_extra_opts(_, undefined) -> [];
get_extra_opts(Listener, App) when Listener == https; Listener == spdy ->
    Opts = leptus_config:lookup(Listener),
    PrivDir = leptus_config:priv_dir(App),
    [
     {cacertfile, filename:join(PrivDir, get_value(cacertfile, Opts))},
     {certfile, filename:join(PrivDir, get_value(certfile, Opts))},
     {keyfile, filename:join(PrivDir, get_value(keyfile, Opts))}
    ];
get_extra_opts(_, _) -> [].
