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

-module(leptus_handler).

%% cowboy callbacks
-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-include("leptus.hrl").

%% -----------------------------------------------------------------------------
%% types
%% -----------------------------------------------------------------------------
-type req() :: cowboy_req:req().
-type status() :: non_neg_integer() | binary() | atom().
-type headers() :: cowboy:http_headers().
-type body() :: binary() | string() | {json | msgpack, json_term()} | {html, binary()}.
-type method() :: get | put | post | delete.
-type json_term() :: [json_term()]
                   | {binary() | atom(), json_term()}
                   | true
                   | false
                   | null
                   | integer()
                   | float()
                   | binary().
-type response() :: {body(), handler_state()}
                  | {status(), body(), handler_state()}
                  | {status(), headers(), body(), handler_state()}.
-type terminate_reason() :: {normal, timeout | shutdown} | {error, atom()}.
-type data_format() :: text | json | msgpack | html.
-type status_code() :: 100..101 | 200..206 | 300..307 | 400..417 | 500..505.

%% -----------------------------------------------------------------------------
%% cowboy callbacks
%% -----------------------------------------------------------------------------
-spec init({module(), http}, Req, Ctx) ->
                  {ok, Req, Ctx} when Req :: req(), Ctx :: ctx().
init(_Transport, Req, Ctx=#ctx{route=Route, handler=Handler,
                               handler_state=HandlerState}) ->
    {ok, ReqPid} = leptus_req:start_link(Req),
    {ok, HandlerState1} = handler_init(Handler, Route, ReqPid, HandlerState),
    {ok, Req, Ctx#ctx{handler_state = HandlerState1, req_pid = ReqPid}}.

-spec handle(Req, Ctx) -> {ok, Req, Ctx} when Req :: req(), Ctx :: ctx().
handle(_Req, Ctx) ->
    %% convert the http method to a lowercase atom
    Func = http_method(leptus_req:method(Ctx#ctx.req_pid)),
    handle_request(Func, Ctx).

-spec terminate(terminate_reason(), req(), ctx()) -> ok.
terminate(Reason, _Req, Ctx) ->
    Res = handler_terminate(Reason, Ctx),
    leptus_req:stop(Ctx#ctx.req_pid),
    Res.

%% -----------------------------------------------------------------------------
%% internal
%% -----------------------------------------------------------------------------
-spec is_defined(module(), atom()) -> boolean().
is_defined(Handler, Func) ->
    erlang:function_exported(Handler, Func, 3).

-spec http_method(binary()) -> method() | not_allowed.
http_method(<<"GET">>) -> get;
http_method(<<"PUT">>) -> put;
http_method(<<"POST">>) -> post;
http_method(<<"DELETE">>) -> delete;
http_method(_) -> not_allowed.

%% -----------------------------------------------------------------------------
%% Handler:init/3
%% -----------------------------------------------------------------------------
-spec handler_init(handler(), route(), Req, handler_state()) ->
                          {ok, handler_state()} when Req :: req().
handler_init(Handler, Route, Req, HandlerState) ->
    Handler:init(Route, Req, HandlerState).

%% -----------------------------------------------------------------------------
%% Handler:Method/3 (Method :: get | put | post | delete)
%% -----------------------------------------------------------------------------
-spec handle_request(method() | not_allowed, Ctx) ->
                            {ok, Req, Ctx} when Req :: req(), Ctx :: ctx().
handle_request(not_allowed, Ctx=#ctx{handler=Handler, route=Route,
                                     handler_state=HandlerState}) ->
    Response = method_not_allowed(Handler, Route, HandlerState),
    reply(Response, Ctx);
handle_request(Func, Ctx=#ctx{handler=Handler, route=Route,
                              handler_state=HandlerState, req_pid=Req}) ->
    Response = case is_allowed(Handler, Func, Route, Req) of
                   true ->
                       case handler_is_authorized(Handler, Route, Req, HandlerState) of
                           {true, HandlerState1} ->
                               Handler:Func(Route, Req, HandlerState1);
                           {false, Res} ->
                               Res
                       end;
                   false ->
                       method_not_allowed(Handler, Route, HandlerState)
               end,
    reply(Response, Ctx).

%% -----------------------------------------------------------------------------
%% Handler:is_authorized/3
%% -----------------------------------------------------------------------------
-spec handler_is_authorized(handler(), route(), req(), handler_state()) ->
                                   {true, handler_state()} | {false, response()}.
handler_is_authorized(Handler, Route, Req, HandlerState) ->
    %%
    %% spec:
    %%   is_authorized(Route, Req, State) ->
    %%     {true, State} | {false, Body, State} | {false, Headers, Body, State}.
    %%
    case is_defined(Handler, is_authorized) of
        true ->
            case Handler:is_authorized(Route, Req, HandlerState) of
                {true, HandlerState1} ->
                    {true, HandlerState1};
                {false, Body, HandlerState1} ->
                    {false, {401, Body, HandlerState1}};
                {false, Headers, Body, HandlerState1} ->
                    {false, {401, Headers, Body, HandlerState1}}
            end;
        false ->
            {true, HandlerState}
    end.

%% -----------------------------------------------------------------------------
%% Handler:allowed_methods/1
%% check if method allowed
%% -----------------------------------------------------------------------------
-spec is_allowed(handler(), method(), route(), req()) -> boolean().
is_allowed(Handler, Func, Route, Req) ->
    %% check if Handler:Func/3 is exported
    case is_defined(Handler, Func) of
        true ->
            %% check if the http method is existing in allowed methods list
            %%
            %% e.g.
            %%   lists:member(<<"GET">>, [<<"GET">>, <<"DELETE">>])
            %%
            lists:member(leptus_req:method(Req), Handler:allowed_methods(Route));
        false ->
            false
    end.

%% -----------------------------------------------------------------------------
%% Handler:allowed_methods/1
%% 'Method not Allowed' response
%% -----------------------------------------------------------------------------
-spec method_not_allowed(handler(), route(), handler_state()) -> response().
method_not_allowed(Handler, Route, HandlerState) ->
    %%
    %% spec:
    %%   allowed_methods(Route) -> [binary()]
    %% e.g.
    %%   allowed_methods("/") -> [<<"GET">>, <<"POST">>]
    %%
    {405, [{<<"allow">>, join_http_methods(Handler:allowed_methods(Route))}],
     <<>>, HandlerState}.

%% -----------------------------------------------------------------------------
%% Handler:cross_domains/3
%% -----------------------------------------------------------------------------
-spec handler_cross_domains(handler(), route(), req(), handler_state()) ->
                                   headers().
handler_cross_domains(Handler, Route, Req, HandlerState) ->
    %%
    %% spec:
    %%   Handler:cross_domains(Route, Req, State) -> [string()].
    %%
    case leptus_req:header(Req, <<"origin">>) of
        <<>> ->
            [];
        Origin ->
            case is_defined(Handler, cross_domains) of
                false ->
                    [];
                true ->
                    HostMatches = Handler:cross_domains(Route, Req, HandlerState),
                    case origin_matches(Origin, HostMatches) of
                        false ->
                            [];
                        true ->
                            [{<<"access-control-allow-origin">>, Origin}]
                    end
            end
    end.

%% -----------------------------------------------------------------------------
%% Handler:terminate/4
%% -----------------------------------------------------------------------------
-spec handler_terminate(terminate_reason(), ctx()) -> ok.
handler_terminate(Reason, #ctx{handler=Handler, route=Route, req_pid=Req,
                               handler_state=HandlerState}) ->
    Handler:terminate(Reason, Route, Req, HandlerState).

%% -----------------------------------------------------------------------------
%% reply - prepare stauts, headers and body
%% -----------------------------------------------------------------------------
-spec reply(response(), Ctx) -> {ok, Req, Ctx} when Req :: req(), Ctx :: ctx().
reply({Body, HandlerState}, Ctx) ->
    reply(200, [], Body, Ctx#ctx{handler_state = HandlerState});
reply({Status, Body, HandlerState}, Ctx) ->
    reply(Status, [], Body, Ctx#ctx{handler_state = HandlerState});
reply({Status, Headers, Body, HandlerState}, Ctx) ->
    reply(Status, Headers, Body, Ctx#ctx{handler_state = HandlerState}).

-spec reply(status(), headers(), body(), Ctx) ->
                   {ok, Req, Ctx} when Req :: req(), Ctx :: ctx().
reply(Status, Headers, Body, Ctx=#ctx{handler=Handler, route=Route, req_pid=Req,
                                      handler_state=HandlerState}) ->
    %% encode Body and set content-type
    {Headers1, Body1} = prepare_headers_body(Headers, Body),

    %% enable or disable cross-domain requests
    Headers2 = Headers1 ++ handler_cross_domains(Handler, Route, Req,
                                                 HandlerState),
    Req1 = leptus_req:get_req(Req),
    {ok, Req2} = cowboy_req:reply(status(Status), Headers2, Body1, Req1),
    leptus_req:set_req(Req, Req2),
    {ok, Req2, Ctx}.

-spec prepare_headers_body(headers(), body()) -> {headers(), body()}.
prepare_headers_body(Headers, {json, Body}) ->
    {maybe_set_content_type(json, Headers), leptus_json:encode(Body)};
prepare_headers_body(Headers, {msgpack, Body}) ->
    {maybe_set_content_type(msgpack, Headers), msgpack:pack({Body}, [jiffy])};
prepare_headers_body(Headers, {html, Body}) ->
    {maybe_set_content_type(html, Headers), Body};
prepare_headers_body(Headers, Body) ->
    {maybe_set_content_type(text, Headers), Body}.

-spec maybe_set_content_type(data_format(), headers()) -> headers().
maybe_set_content_type(Type, Headers) ->
    Headers1 = [{cowboy_bstr:to_lower(N), V} || {N, V} <- Headers],
    %% don't set content-type if it's already been set
    case lists:keyfind(<<"content-type">>, 1, Headers1) of
        {_, _} ->
            Headers;
        _ ->
            [{<<"content-type">>, content_type(Type)}|Headers]
    end.

-spec content_type(data_format()) -> binary().
content_type(text) -> <<"text/plain">>;
content_type(html) -> <<"text/html">>;
content_type(json) -> <<"application/json">>;
content_type(msgpack) -> <<"application/x-msgpack">>.

%% -----------------------------------------------------------------------------
%% HTTP status code bindings
%% -----------------------------------------------------------------------------
-spec status(atom() | A) -> status_code() | A when A :: any().
%% informational
status(continue) -> 100;
status(switching_protocols) -> 101;
%% successful
status(ok) -> 200;
status(created) -> 201;
status(accepted) -> 202;
status(non_authoritative_information) -> 203;
status(no_content) -> 204;
status(reset_content) -> 205;
status(partial_content) -> 206;
%% redirection
status(multiple_choices) -> 300;
status(moved_permanently) -> 301;
status(found) -> 302;
status(see_other) -> 303;
status(not_modified) -> 304;
status(use_proxy) -> 305;
status(switch_proxy) -> 306;
status(temporary_redirect) -> 307;
%% client error
status(bad_request) -> 400;
status(unauthorized) -> 401;
status(payment_required) -> 402;
status(forbidden) -> 403;
status(not_found) -> 404;
status(not_allowed) -> 405;
status(not_acceptable) -> 406;
status(proxy_authentication_required) -> 407;
status(request_timeout) -> 408;
status(conflict) -> 409;
status(gone) -> 410;
status(length_required) -> 411;
status(precondition_failed) -> 412;
status(request_entity_too_large) -> 413;
status(request_uri_too_long) -> 414;
status(unsupported_media_type) -> 415;
status(requested_range_not_satisfiable) -> 416;
status(expectation_failed) -> 417;
%% server error
status(internal_server_error) -> 500;
status(not_implemented) -> 501;
status(bad_gateway) -> 502;
status(service_unavailable) -> 503;
status(gateway_timeout) -> 504;
status(http_version_not_supported) -> 505;
status(A) -> A.

-spec join_http_methods([binary()]) -> binary().
join_http_methods(Methods) ->
    <<", ", Allow/binary>> = << <<", ", M/binary>> || M <- Methods >>,
    Allow.

-spec compile_host(string() | binary()) -> [[binary() | atom()]] | [atom()].
compile_host(HostMatch) ->
    [X || {X, _, _} <- cowboy_router:compile([{HostMatch, []}])].

-spec origin_matches(binary(), [atom() | string() | binary()]) -> boolean().
origin_matches(Origin, HostMatches) ->
    %% [<<"com">>, <<"example">>], "example.com", [...]
    domains_match(hd(compile_host(Origin)), HostMatches).

%% TODO: write tests
domains_match(_, []) ->
    false;
domains_match(OriginToks, [HostMatch|Rest]) ->
    %% [<<"com">>, <<"example">>], [[<<"com">>, <<"example">>], ...], [...]
    domains_match(OriginToks, compile_host(HostMatch), Rest, OriginToks).

domains_match(_, ['_'], _, _) ->
    true;
domains_match(OriginToks, [HMToks|Rest], HostMatches, OriginToks) ->
    domain_matches(OriginToks, HMToks, Rest, HostMatches, OriginToks).

domain_matches(OriginToks, OriginToks, _, _, _) ->
    true;
domain_matches(_, ['...'|_], _, _, _) ->
    true;
domain_matches([], [], _, _, _) ->
    true;
domain_matches([_|T], ['_'|HMToks], Rest, HostMatches, OriginToksReplica) ->
    domain_matches(T, HMToks, Rest, HostMatches, OriginToksReplica);
domain_matches([H|T], [H|HMToks], Rest, HostMatches, OriginToksReplica) ->
    domain_matches(T, HMToks, Rest, HostMatches, OriginToksReplica);
domain_matches(_, _, [HMToks|Rest], HostMatches, OriginToksReplica) ->
    domain_matches(OriginToksReplica, HMToks, Rest, HostMatches, OriginToksReplica);
domain_matches(_, _, [], [], _) ->
    false;
domain_matches(_, _, [], HostMatches, OriginToks) ->
    domains_match(OriginToks, HostMatches).
