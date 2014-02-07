%% This file is part of leptus, and released under the MIT license.
%% See LICENSE for more information.

-module(pt2).
-compile({parse_transform, leptus_pt}).
-export([get/3]).
-export([put/3]).
-export([post/3]).

get("/1", _, State) ->
    {200, <<>>, State}.

put("/2", _, State) ->
    {201, <<>>, State};
put("/3", _, State) ->
    {200, <<>>, State}.

post("/1", _, State) ->
    {401, <<"blah">>, State};
post("/4", _, State) ->
    {201, <<>>, State}.
