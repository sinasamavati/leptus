-module(leptus_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================
init([]) ->
    {ok, {
       {one_for_one, 5, 10}, [child(leptus_router, worker)]}
    }.


%% helper function for declaring children of supervisor
child(I, Type) ->
    child(I, Type, []).

child(I, Type, Args) ->
    {I, {I, start_link, Args}, permanent, 5000, Type, [I]}.
