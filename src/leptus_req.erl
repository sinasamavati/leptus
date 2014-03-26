%% The MIT License

%% Copyright (c) 2013-2014 Sina Samavati <sina.samv@gmail.com>

%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:

%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.

%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.

%% a bunch of functions to deal with a request
-module(leptus_req).
-behaviour(gen_server).

%% -----------------------------------------------------------------------------
%% gen_server callbacks
%% -----------------------------------------------------------------------------
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

%% -----------------------------------------------------------------------------
%% API
%% -----------------------------------------------------------------------------
-export([start_link/1]).
-export([start/1]).
-export([stop/1]).
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

-spec start_link(cowboy_req:req()) -> {ok, pid()} | ignore | {error, any()}.
start_link(Req) ->
    gen_server:start_link(?MODULE, Req, []).

-spec start(cowboy_req:req()) -> {ok, pid()} | ignore | {error, any()}.
start(Req) ->
    gen_server:start(?MODULE, Req, []).

-spec stop(pid()) -> ok.
stop(Pid) ->
    gen_server:call(Pid, stop).

-spec param(pid(), atom()) -> binary() | undefined.
param(Pid, Key) ->
    gen_server:call(Pid, {binding, [Key]}).

-spec params(pid()) -> [{atom(), binary()}] | undefined.
params(Pid) ->
    gen_server:call(Pid, {bindings, []}).

-spec qs(pid()) -> binary().
qs(Pid) ->
    gen_server:call(Pid, {qs, []}).

-spec qs_val(pid(), binary()) -> binary() | undefined.
qs_val(Pid, Key) ->
    gen_server:call(Pid, {qs_val, [Key]}).

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

%% -----------------------------------------------------------------------------
%% gen_server callbacks
%% -----------------------------------------------------------------------------
init(Req) ->
    {ok, Req}.

handle_call(stop, _From, Req) ->
    {stop, shutdown, ok, Req};
handle_call({F, A}, _From, Req) ->
    {Value, Req1} = invoke(F, A ++ [Req]),
    {reply, Value, Req1}.

handle_cast(_Msg, Req) ->
    {noreply, Req}.

handle_info(_Info, Req) ->
    {noreply, Req}.

terminate(_Reason, _Req) ->
    ok.

code_change(_OldVsn, Req, _Extra) ->
    {ok, Req}.

%% -----------------------------------------------------------------------------
%% internal
%% -----------------------------------------------------------------------------
invoke(F, A) ->
    get_vr(apply(cowboy_req, F, A)).

%% get value and req
get_vr(Res={_, _}) ->
    Res;
get_vr({ok, Value, Req}) ->
    {Value, Req};
get_vr({undefined, Value, Req}) ->
    {Value, Req}.

%% TODO: complete the new api
%% leptus_req should be a gen_server and it should require Pid
%% TO BE CONSIDERED: each function call updates the state (Req)
%% so what about a number of concurrent function calls?
%% last write wins?
