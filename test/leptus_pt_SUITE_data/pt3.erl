-module(pt3).
-compile({parse_transform, leptus_pt}).

-export([get/2]).
-export([put/2]).
-export([post/2]).
-export([delete/2]).

get("/", _) ->
    {200, <<>>}.

put("/old", _) ->
    {200, <<>>}.

post("/new", _) ->
    {201, <<>>}.

delete("/old", _) ->
    {204, <<>>}.
