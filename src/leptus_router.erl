-module(leptus_router).

-behaviour(gen_server).

%% gen_server
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-export([start_link/0]).
-export([stop/0]).
-export([fetch_routes/1]).
-export([dispatches/1]).
-export([find_mod/1]).

-type req_route() :: string().
-type routes() :: [{module(), [req_route()]}].


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:cast(?MODULE, stop).

-spec fetch_routes([module()]) -> routes().
fetch_routes(Mods) ->
    gen_server:call(?MODULE, {fetch_routes, Mods}).

-spec find_mod(string()) -> {ok, module()} | {error, undefined}.
find_mod(Route) ->
    gen_server:call(?MODULE, {find_mod, Route}).

%% gen_server
init([]) ->
    {ok, []}.

handle_call({fetch_routes, Mods}, _From, _State) ->
    NewState = fetch_routes(Mods, []),
    {reply, NewState, NewState};

handle_call({find_mod, Route}, _From, State) ->
    {reply, find_mod(Route, State), State}.

handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(normal, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% internal
fetch_routes([], Acc) ->
    Acc;
fetch_routes([Mod|T], Acc) ->
    %% each module must have routes/0 -> [string()].
    Routes = apply(Mod, routes, []),
    fetch_routes(T, orddict:append_list(Mod, Routes, Acc)).

-spec dispatches([module()]) -> cowboy_router:dispatch_rules().
dispatches(Mods) ->
    [{'_', fetch_paths(Mods)}].

-spec fetch_paths([module()]) -> [cowboy_router:route_path()].
fetch_paths(Mods) ->
    Routes = fetch_routes(Mods),
    handle_routes(Routes).

-spec handle_routes(routes()) -> [cowboy_router:route_path()].
handle_routes(Routes) ->
    Values = orddict:fold(fun(_, V, AccIn) -> AccIn ++ V end, [], Routes),
    handle_routes(Values, []).

handle_routes([], Acc) ->
    Acc;
handle_routes([Route|T], Acc) ->
    handle_routes(T, Acc ++ [{Route, leptus_resouce_handler, Route}]).

-spec find_mod(req_route(), routes()) -> {ok, string()} | {error, undefined}.
find_mod(Route, Routes) ->
    Mod = orddict:fold(fun(K, V, Acc) ->
                               case lists:member(Acc, V) of
                                   true ->
                                       K;
                                   false ->
                                       Acc
                               end
                       end, Route, Routes),

    if Mod =/= Route ->
            {ok, Mod};
       true ->
            {error, undefined}
    end.
