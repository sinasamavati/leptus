%% a bunch of functions to deal with a request
-module(leptus_req).

-export([binding/2]).
-export([bindings/1]).
-export([qs/1]).
-export([qs_val/2]).
-export([uri/1]).
-export([version/1]).

-type req() :: cowboy_req:req().


-spec binding(atom(), req()) -> binary() | undefined.
binding(Key, Req) ->
    get_value(cowboy_req:binding(Key, Req)).

-spec bindings(req()) -> [{atom(), binary()}] | undefined.
bindings(Req) ->
    get_value(cowboy_req:bindings(Req)).

-spec qs(req()) -> binary().
qs(Req) ->
    get_value(cowboy_req:qs(Req)).

-spec qs_val(binary(), req()) -> binary() | undefined.
qs_val(Key, Req) ->
    get_value(cowboy_req:qs_val(Key, Req)).

-spec uri(req()) -> binary().
uri(Req) ->
    Path = get_value(cowboy_req:path(Req)),
    QS = qs(Req),

    %% e.g <<"/path?query=string">>

    case QS of
        <<>> -> Path;
        _ -> <<Path/binary, <<"?">>/binary, QS/binary>>
    end.

-spec version(req()) -> cowboy:http_version().
version(Req) ->
    get_value(cowboy_req:version(Req)).


%% internal
get_value({Value, _}) ->
    Value.
