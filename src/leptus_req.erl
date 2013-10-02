%% a bunch of functions to deal with a request
-module(leptus_req).

-export([binding/2]).
-export([bindings/1]).
-export([qs/1]).
-export([qs_val/2]).
-export([uri/1]).
-export([version/1]).
-export([body/1]).
-export([body_raw/1]).
-export([body_qs/1]).
-export([header/2]).

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

-spec body(req()) -> binary().
body(Req) ->
    Body = body_raw(Req),
    case header(<<"content-type">>, Req) of
        %% decode body if content-type is json
        <<"application/json">> ->
            jsx:decode(Body);
        _ ->
            Body
    end.

-spec body_raw(req()) -> binary().
body_raw(Req) ->
    get_value(cowboy_req:body(infinity, Req)).

-spec body_qs(req()) -> [{binary(), binary() | true}].
body_qs(Req) ->
    get_value(cowboy_req:body_qs(infinity, Req)).

-spec header(binary(), req()) -> binary().
header(Name, Req) ->
    get_value(cowboy_req:header(Name, Req, <<>>)).


%% internal
get_value({Value, _}) ->
    Value;
get_value({ok, Value, _}) ->
    Value.
