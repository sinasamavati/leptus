%% a bunch of functions to deal with a request
-module(leptus_req).

-export([param/2]).
-export([params/1]).
-export([qs/1]).
-export([qs_val/2]).
-export([uri/1]).
-export([version/1]).
-export([method/1]).
-export([body/1]).
-export([body_raw/1]).
-export([body_qs/1]).
-export([header/2]).
-export([parse_header/2]).
-export([auth/2]).

-include("leptus.hrl").


-spec param(atom(), Req) -> binary() | undefined when Req::req().
param(Key, Req) ->
    invoke(binding, [Key, Req]).

-spec params(Req) -> [{atom(), binary()}] | undefined when Req::req().
params(Req) ->
    Req#http_req.bindings.

-spec qs(Req) -> binary() when Req::req().
qs(Req) ->
    Req#http_req.qs.

-spec qs_val(binary(), Req) -> binary() | undefined when Req::req().
qs_val(Key, Req) ->
    invoke(qs_val, [Key, Req]).

-spec uri(Req) -> binary() when Req::req().
uri(Req) ->
    Path = Req#http_req.path,
    QS = qs(Req),

    %% e.g <<"/path?query=string">>

    case QS of
        <<>> -> Path;
        _ -> <<Path/binary, "?", QS/binary>>
    end.

-spec version(Req) -> cowboy:http_version() when Req::req().
version(Req) ->
    Req#http_req.version.

-spec method(Req) -> binary() when Req::req().
method(Req) ->
    Req#http_req.method.

-spec body(cowboy_req:req()) -> binary().
body(Req) ->
    Body = body_raw(Req),
    case header(<<"content-type">>, Req) of
        %% decode body if content-type is json
        <<"application/json">> ->
            leptus_json:decode(Body);
        _ ->
            Body
    end.

-spec body_raw(cowboy_req:req()) -> binary().
body_raw(Req) ->
    invoke(body, [infinity, Req]).

-spec body_qs(cowboy_req:req()) -> [{binary(), binary() | true}].
body_qs(Req) ->
    invoke(body_qs, [infinity, Req]).

-spec header(binary(), cowboy_req:req()) -> binary().
header(Name, Req) ->
    invoke(header, [Name, Req, <<>>]).

-spec parse_header(binary(), cowboy_req:req()) -> any() | <<>>.
parse_header(Name, Req) ->
    invoke(parse_header, [Name, Req, <<>>]).

-spec auth(binary(), cowboy_req:req()) -> {binary(), binary()} | <<>> | error.
auth(<<"basic">>, Req) ->
    case parse_header(<<"authorization">>, Req) of
        {<<"basic">>, UserPass} ->
            UserPass;
        Value ->
            Value
    end.


%% internal
invoke(F, A) ->
    get_value(apply(cowboy_req, F, A)).

get_value({Value, _}) ->
    Value;
get_value({ok, Value, _}) ->
    Value;
get_value({undefined, Value, _}) ->
    Value.
