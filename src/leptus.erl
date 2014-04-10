%% The MIT License

%% Copyright (c) 2013-2014 Sina Samavati <sina.samv@gmail.com>

%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:

%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.

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
-export([stop_listener/1]).

%% -----------------------------------------------------------------------------
%% types
%% -----------------------------------------------------------------------------
-type host_match() :: term().
-type handler() :: {module(), State :: any()}.
-type handlers() :: [{host_match(), [handler()]}].
-export_type([handler/0]).
-export_type([handlers/0]).

-type listener() :: http | https | spdy.
-type option() :: {nb_acceptors, non_neg_integer()}
                | {ip, inet:ip_address()}
                | {port, inet:port_number()}
                | {cacertfile, file:name_all()}
                | {certfile, file:name_all()}
                | {keyfile, file:name_all()}.
-type options() :: [option()].
-export_type([listener/0]).
-export_type([options/0]).


%% -----------------------------------------------------------------------------
%% API
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
    ensure_deps_started(),
    ensure_started(leptus),

    %% routes
    Paths = leptus_router:paths(Handlers),
    Dispatch = cowboy_router:compile(Paths),
    %% sort compiled routes
    Dispatch1 = leptus_router:sort_dispatch(Dispatch),

    ListenerFunc = get_listener_func(Listener),
    Ref = get_ref(Listener),
    NbAcceptors = opt(nb_acceptors, Opts, 100),

    %% basic listener configuration
    IP = opt(ip, Opts, {127, 0, 0, 1}),
    Port = opt(port, Opts, 8080),

    ListenerOpts = listener_opts(Listener, IP, Port, Opts),
    Res = cowboy:ListenerFunc(Ref, NbAcceptors, ListenerOpts,
                              [
                               {env, [{dispatch, Dispatch1}]},
                               {onresponse, fun leptus_hooks:console_log/4}
                              ]),
    case Res of
        {ok, _} ->
            print_info(IP, Port);
        _ ->
            ok
    end,
    Res.

-spec stop_listener(listener()) -> ok | {error, not_found}.
stop_listener(Listener) ->
    cowboy:stop_listener(get_ref(Listener)).

%% -----------------------------------------------------------------------------
%% internal
%% -----------------------------------------------------------------------------
-spec get_listener_func(listener()) -> atom().
get_listener_func(http) -> start_http;
get_listener_func(https) -> start_https;
get_listener_func(spdy) -> start_spdy.

-spec get_ref(listener()) -> ranch:ref().
get_ref(http) -> leptus_http;
get_ref(https) -> leptus_https;
get_ref(spdy) -> leptus_spdy.

%% -----------------------------------------------------------------------------
%% listener options
%% -----------------------------------------------------------------------------
-spec listener_opts(listener(), inet:ip_address(), inet:port_number(),
                    options()) -> options().
listener_opts(http, IP, Port, _) ->
    basic_listener_opts(IP, Port);
listener_opts(_, IP, Port, Opts) ->
    basic_listener_opts(IP, Port) ++ extra_listener_opts(Opts).

-spec basic_listener_opts(inet:ip_address(), inet:port_number()) -> options().
basic_listener_opts(IP, Port) ->
    [{ip, IP}, {port, Port}].

-spec extra_listener_opts(options()) -> options().
extra_listener_opts(Opts) ->
    [
     {cacertfile, opt(cacertfile, Opts, "")},
     {certfile, opt(certfile, Opts, "")},
     {keyfile, opt(keyfile, Opts, "")}
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

opt(Key, [{Key, Value}|_], _) ->
    Value;
opt(Key, [_|Rest], Default) ->
    opt(Key, Rest, Default);
opt(_, [], Default) ->
    Default.

inet_ip_to_str({A, B, C, D}) ->
    lists:concat([A, ".", B, ".", C, ".", D]).

%% -----------------------------------------------------------------------------
%% print the version number and what ip/port it's started on
%% -----------------------------------------------------------------------------
print_info(IP, Port) ->
    {ok, Vsn} = application:get_key(leptus, vsn),
    io:format("Leptus ~s started on http://~s:~p~n",
              [Vsn, inet_ip_to_str(IP), Port]).
