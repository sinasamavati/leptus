%% a bunch of functions to deal with a request
-module(leptus_req).

-export([binding/2]).
-export([bindings/1]).


binding(Key, Req) ->
    case cowboy_req:binding(Key, Req) of
        {undefined, _} ->
            undefined;
        {Value, _} ->
            Value
    end.

bindings(Req) ->
    case cowboy_req:bindings(Req) of
        {undefined, _} ->
            undefined;
        {Params, _} ->
            Params
    end.
