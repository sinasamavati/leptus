%% Copyright (c) 2013-2015 Sina Samavati <sina.samv@gmail.com>
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
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
-export([header/3]).
-export([parse_header/2]).
-export([auth/2]).
-export([peer/1]).
-export([reply/4]).
-export([get_req/1]).
-export([set_req/2]).

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
    invoke(Pid, binding, [Key]).

-spec params(pid()) -> [{atom(), binary()}] | undefined.
params(Pid) ->
    invoke(Pid, bindings, []).

-spec qs(pid()) -> binary().
qs(Pid) ->
    invoke(Pid, qs, []).

-spec qs_val(pid(), binary()) -> binary() | undefined.
qs_val(Pid, Key) ->
    invoke(Pid, qs_val, [Key]).

-spec uri(pid()) -> binary().
uri(Pid) ->
    Path = invoke(Pid, path, []),
    QS = qs(Pid),

    %% e.g <<"/path?query=string">>
    case QS of
        <<>> -> Path;
        _ -> <<Path/binary, "?", QS/binary>>
    end.

-spec version(pid()) -> cowboy:http_version().
version(Pid) ->
    invoke(Pid, version, []).

-spec method(pid()) -> binary().
method(Pid) ->
    invoke(Pid, method, []).

-spec body(pid()) -> binary() | leptus_json:json_term().
body(Pid) ->
    Body = body_raw(Pid),
    case header(Pid, <<"content-type">>) of
        %% decode body if content-type is json or msgpack
        <<"application/json">> ->
            try leptus_json:decode(Body) of
                {_, _} -> Body;
                Term -> Term
            catch _:_ -> Body
            end;
        <<"application/x-msgpack">> ->
            case msgpack:unpack(Body) of
                {ok, {UnpackedBody}} ->
                    UnpackedBody;
                _ ->
                    Body
            end;
        _ ->
            Body
    end.

-spec body_raw(pid()) -> binary().
body_raw(Pid) ->
    invoke(Pid, body, [infinity]).

-spec body_qs(pid()) -> [{binary(), binary() | true}].
body_qs(Pid) ->
    invoke(Pid, body_qs, [infinity]).

-spec header(pid(), binary()) -> binary() | undefined.
header(Pid, Name) ->
    invoke(Pid, header, [Name]).

-spec header(pid(), binary(), Default) -> binary() | Default when Default :: any().
header(Pid, Name, Default) ->
    invoke(Pid, header, [Name, Default]).

-spec parse_header(pid(), binary()) -> any() | undefined | {error, any()}.
parse_header(Pid, Name) ->
    invoke(Pid, parse_header, [Name, undefined]).

-spec auth(pid(), basic) -> {binary(), binary()} | undefined | {error, any()}.
auth(Pid, basic) ->
    case parse_header(Pid, <<"authorization">>) of
        {<<"basic">>, UserPass} ->
            UserPass;
        Value ->
            Value
    end.

-spec peer(pid()) -> {inet:ip_address(), inet:port_number()}.
peer(Pid) ->
    invoke(Pid, peer, []).

-spec reply(pid(), cowboy:http_status(), cowboy:http_headers(), iodata()) -> ok.
reply(Pid, Status, Headers, Body) ->
    invoke(Pid, reply, [Status, Headers, Body]).

-spec get_req(pid()) -> cowboy_req:req().
get_req(Pid) ->
    gen_server:call(Pid, get_req).

-spec set_req(pid(), cowboy_req:req()) -> ok.
set_req(Pid, Req) ->
    gen_server:cast(Pid, {set_req, Req}).

%% -----------------------------------------------------------------------------
%% gen_server callbacks
%% -----------------------------------------------------------------------------
init(Req) ->
    {ok, Req}.

handle_call(stop, _From, Req) ->
    {stop, shutdown, ok, Req};
handle_call(get_req, _From, Req) ->
    {reply, Req, Req};
handle_call({F, A}, _From, Req) ->
    {Value, Req1} = case call_cowboy_req(F, A, Req) of
                        Err = {error, _} -> {Err, Req};
                        Else -> Else
                    end,
    {reply, Value, Req1}.

handle_cast({set_req, NewReq}, _Req) ->
    {noreply, NewReq};
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
-spec invoke(pid(), atom(), [any()]) -> any().
invoke(Pid, F, A) ->
    gen_server:call(Pid, {F, A}).

-spec call_cowboy_req(atom(), [any()], cowboy_req:req()) -> any().
call_cowboy_req(reply, Args, Req) ->
    call_cowboy_req(reply, Args ++ [Req]);
call_cowboy_req(F, [], Req) ->
    call_cowboy_req(F, [Req]);
call_cowboy_req(F, [H|T], Req) ->
    A = [H] ++ [Req|T],
    call_cowboy_req(F, A).

-spec call_cowboy_req(atom(), [any()]) -> any().
call_cowboy_req(F, A) ->
    get_vr(apply(cowboy_req, F, A)).

%% get value and req
-spec get_vr({atom(), any(), cowboy_req:req()} | {any(), cowboy_req:req()}) ->
                    {any(), cowboy_req:req()}.
get_vr(Res={_, _}) ->
    Res;
get_vr({ok, Value, Req}) ->
    {Value, Req};
get_vr({undefined, Value, Req}) ->
    {Value, Req}.
