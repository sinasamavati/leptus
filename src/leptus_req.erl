%% a bunch of functions to deal with a request
-module(leptus_req).

-export([binding/2]).
-export([bindings/1]).

-type req() :: cowboy_req:req().


-spec binding(atom(), req()) -> binary() | undefined.
binding(Key, Req) ->
    get_value(cowboy_req:binding(Key, Req)).

-spec bindings(req()) -> [{atom(), binary()}] | undefined.
bindings(Req) ->
    get_value(cowboy_req:bindings(Req)).


%% internal
get_value({Value, _}) ->
    Value.
