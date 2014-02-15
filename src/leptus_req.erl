%% This file is part of leptus, and released under the MIT license.
%% See LICENSE for more information.

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


-spec param(atom(), cowboy_req:req()) -> binary() | undefined.
param(Key, Req) ->
    invoke(binding, [Key, Req]).

-spec params(cowboy_req:req()) -> [{atom(), binary()}] | undefined.
params(Req) ->
    invoke(bindings, [Req]).

-spec qs(cowboy_req:req()) -> binary().
qs(Req) ->
    invoke(qs, [Req]).

-spec qs_val(binary(), cowboy_req:req()) -> binary() | undefined.
qs_val(Key, Req) ->
    invoke(qs_val, [Key, Req]).

-spec uri(cowboy_req:req()) -> binary().
uri(Req) ->
    Path = invoke(path, [Req]),
    QS = invoke(qs, [Req]),

    %% e.g <<"/path?query=string">>

    case QS of
        <<>> -> Path;
        _ -> <<Path/binary, "?", QS/binary>>
    end.

-spec version(cowboy_req:req()) -> cowboy:http_version().
version(Req) ->
    invoke(version, [Req]).

-spec method(cowboy_req:req()) -> binary().
method(Req) ->
    invoke(method, [Req]).

-spec body(cowboy_req:req()) -> binary() | leptus_handler:json_term() |
                                msgpack:msgpack_term() | {error, any()}.
body(Req) ->
    Body = body_raw(Req),
    case header(<<"content-type">>, Req) of
        %% decode body if content-type is json or msgpack
        <<"application/json">> ->
            leptus_json:decode(Body);
        <<"application/x-msgpack">> ->
            case msgpack:unpack(Body) of
                {ok, {UnpackedBody}} ->
                    UnpackedBody;
                Else ->
                    Else
            end;
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

-spec auth(basic, cowboy_req:req()) -> {binary(), binary()} | <<>> | error.
auth(basic, Req) ->
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
