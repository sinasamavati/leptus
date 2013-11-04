-module(pt2).
-compile({parse_transform, leptus_pt}).
-export([get/2]).
-export([put/2]).
-export([post/2]).

get("/1", _) ->
    {200, <<>>}.

put("/2", _) ->
    {201, <<>>};
put("/3", _) ->
    {200, <<>>}.

post("/1", _) ->
    {401, <<"blah">>};
post("/4", _) ->
    {201, <<>>}.
