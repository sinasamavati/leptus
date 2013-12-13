%% @author Sina Samavati <sina.samv@gmail.com>

-module(leptus).
-author("Sina Samavati <sina.samv@gmail.com>").

-export([start_http/0]).
-export([start_http/1]).
-export([start_http/2]).
-export([stop_http/0]).
-export([upgrade/0]).
-export([upgrade/1]).

-type handler() :: {module(), State::any()}.
-type handlers() :: [handler()].
-export_type([handlers/0]).


-spec start_http() -> {ok, pid()} | {error, any()}.
start_http() ->
    ensure_deps_started(),
    ensure_started(leptus),
    start_http(leptus_config:handlers(), []).

-spec start_http(handlers()) -> {ok, pid()} | {error, any()}.
start_http(Handlers) ->
    start_http(Handlers, []).

-spec start_http(handlers(), cowboy_protocol:opts()) -> {ok, pid()} | {error, any()}.
start_http(Handlers, ProtoOpts) ->
    ensure_deps_started(),
    ensure_started(leptus),

    leptus_config:set(handlers, Handlers),
    leptus_config:set(proto_opts, ProtoOpts),

    %% routes
    Paths = leptus_router:paths(Handlers),
    Dispatch = cowboy_router:compile([{'_', Paths}]),

    %% basic http configuration
    IP = leptus_config:ip_addr(),
    Port = leptus_config:port_num(),

    cowboy:start_http(
      leptus_http, 100, [{ip, IP}, {port, Port}],
      [
       {env, [{dispatch, Dispatch}]},
       {onresponse, fun leptus_hooks:console_log/4}
      ] ++ ProtoOpts
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
