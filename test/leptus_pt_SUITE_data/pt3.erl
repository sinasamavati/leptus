-module(pt3).
-compile({parse_transform, leptus_pt}).

%% leptus callbacks
-export([init/3]).
-export([get/3]).
-export([put/3]).
-export([post/3]).
-export([delete/3]).
-export([terminate/3]).

init(_Route, _Req, _State) ->
    {ok, state1}.

get("/", _, State) ->
    State = state1,
    {200, <<>>, state2}.

put("/old", _, State) ->
    State = state1,
    {200, <<>>, state3}.

post("/new", _, State) ->
    State = state1,
    {201, <<>>, state4}.

delete("/old", _, State) ->
    State = state1,
    {204, <<>>, State}.

terminate(_Reason, _Req, State) ->
    case State of
        state1 -> ok;
        state2 -> ok;
        state3 -> ok;
        state4 -> ok
    end.
