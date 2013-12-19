%% @author Sina Samavati <sina.samv@gmail.com>

-module(leptus).
-author("Sina Samavati <sina.samv@gmail.com>").

-export([start_http/1]).
-export([start_http/2]).
-export([stop_http/0]).
-export([upgrade/0]).
-export([upgrade/1]).

-type handler() :: {module(), State::any()}.
-type handlers() :: [handler()].
-export_type([handlers/0]).

-type option() :: {priv_dir, atom()}.
-type options() :: [option()].


-spec start_http(handlers()) -> {ok, pid()} | {error, any()}.
start_http(Handlers) ->
    start_http(Handlers, []).

-spec start_http(handlers(), options()) -> {ok, pid()} | {error, any()}.
start_http(Handlers, Options) ->
    ensure_deps_started(),
    ensure_started(leptus),

    leptus_config:set(handlers, Handlers),
    case get_value(priv_dir, Options) of
        undefined ->
            ok;
        App ->
            %% set priv_dir & initialize 'handlers' and 'http' k/v
            leptus_config:set(priv_dir, App),
            %% read leptus.config and insert configurations to ets
            Conf = leptus_config:config_file(App),
            Http = get_value(http, Conf),
            Handlers1 = get_value(handlers, Conf, Handlers),
            leptus_config:set(http, Http),
            leptus_config:set(handlers, Handlers1)
    end,

    %% routes
    Paths = leptus_router:paths(leptus_config:handlers()),
    Dispatch = cowboy_router:compile([{'_', Paths}]),

    %% basic http configuration
    IP = leptus_config:ip_addr(),
    Port = leptus_config:port_num(),

    cowboy:start_http(
      leptus_http, 100, [{ip, IP}, {port, Port}],
      [
       {env, [{dispatch, Dispatch}]},
       {onresponse, fun leptus_hooks:console_log/4}
      ]
     ).

-spec stop_http() -> ok | {error, not_found}.
stop_http() ->
    cowboy:stop_listener(leptus_http).

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
