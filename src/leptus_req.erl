%% a bunch of functions to deal with a request
-module(leptus_req).

-export([binding/2]).
-export([bindings/1]).

-type req() :: cowboy_req:req().


-spec binding(atom(), req()) -> binary() | undefined.
binding(Key, Req) ->
    case cowboy_req:binding(Key, Req) of
        {undefined, _} ->
            undefined;
        {Value, _} ->
            Value
    end.

-spec bindings(req()) -> [{atom(), binary()}] | undefined.
bindings(Req) ->
    case cowboy_req:bindings(Req) of
        {undefined, _} ->
            undefined;
        {Params, _} ->
            Params
    end.
