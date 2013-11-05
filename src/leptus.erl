%% @author Sina Samavati <sina.samv@gmail.com>

-module(leptus).
-author("Sina Samavati <sina.samv@gmail.com>").

-export([start_http/0]).
-export([start_http/1]).
-export([stop_http/0]).


-spec start_http() -> {ok, pid()} | {error, any()}.
start_http() ->
    start_http({modules, get_value(modules, config(), [])}).

-spec start_http({modules, [module()]}) -> {ok, pid()} | {error, any()}.
start_http({modules, Mods}) ->
    %% ensure dependencies are started
    ensure_started(crypto),
    ensure_started(ranch),
    ensure_started(cowboy),
    ensure_started(leptus),

    %% routes
    Paths = leptus_router:paths(Mods),
    Dispatch = cowboy_router:compile([{'_', Paths}]),

    %% basic http configuration
    Config = config(),
    IP = ip_address(Config),
    Port = http_port(Config),

    cowboy:start_http(
      http, 100, [{ip, IP}, {port, Port}],
      [
       {env, [{dispatch, Dispatch}]},
       {onresponse, fun leptus_hooks:console_log/4}
      ]
     ).

-spec stop_http() -> ok | {error, not_found}.
stop_http() ->
    cowboy:stop_listener(http).


%% internal
ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.

%% get IP address to bind to
ip_address(Config) ->
    Default = {127, 0, 0, 1},
    case get_value(http, Config, Default) of
        Default ->
            Default;
        Http ->
            case inet_parse:address(get_value(ip, Http, Default)) of
                {ok, IP} ->
                    IP;
                {error, _} ->
                    Default
            end
    end.

%% get http port to listen on
http_port(Config) ->
    Default = 8080,
    case get_value(http, Config, Default) of
        Default ->
            Default;
        Http ->
            get_value(port, Http, Default)
    end.

%% read priv/leptus.config file
config() ->
    {ok, Cwd} = file:get_cwd(),
    case file:consult(filename:join([Cwd, "priv", "leptus.config"])) of
        {error, _} ->
            [];
        {ok, Terms} ->
            Terms
    end.

get_value(Key, Proplist, Default) ->
    case lists:keyfind(Key, 1, Proplist) of
        {_, Value} -> Value;
        _ -> Default
    end.
