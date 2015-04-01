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

-module(leptus_handler).

%% -----------------------------------------------------------------------------
%% cowboy callbacks
%% -----------------------------------------------------------------------------
-export([init/3]).
-export([upgrade/4]).

-include("leptus.hrl").
-include("leptus_logger.hrl").

%% -----------------------------------------------------------------------------
%% types
%% -----------------------------------------------------------------------------
-type req() :: pid().
-type status() :: non_neg_integer() | binary() | atom().
-type headers() :: cowboy:http_headers().
-type body() :: binary() | string() | {json | msgpack, leptus_json:json_term()}
              | {html, binary()}.
-type method() :: get | put | post | delete.
-type response() :: {body(), handler_state()}
                  | {status(), body(), handler_state()}
                  | {status(), headers(), body(), handler_state()}.
-type terminate_reason() :: normal | not_allowed | unauthenticated
                          | no_permission | {error, any()}.
-type data_format() :: text | json | msgpack | html.
-type status_code() :: 100..101 | 200..206 | 300..307 | 400..417 | 500..505.

-export_type([status/0]).

%% -----------------------------------------------------------------------------
%% internal state record
%% -----------------------------------------------------------------------------
-record(state, {
          resrc = #resrc{} :: resrc(),
          method = <<"GET">> :: binary(),
          terminate_reason = normal :: terminate_reason(),
          log_data = #log_data{} :: log_data()
         }).
-type state() :: #state{}.

%% -----------------------------------------------------------------------------
%% cowboy callbacks
%% -----------------------------------------------------------------------------
init(_, Req, Resrc) ->
    LogData = #log_data{},
    Headers = cowboy_req:get(headers, Req),
    {ok, ReqPid} = leptus_req_sup:start_child(Req),
    Method = leptus_req:method(ReqPid),
    LogData1 = LogData#log_data{method = Method, headers = Headers},
    State = #state{resrc = Resrc, method = Method, log_data = LogData1},
    {upgrade, protocol, ?MODULE, ReqPid, State}.

upgrade(Req, Env, _Handler,
        State=#state{resrc=#resrc{handler=Handler, route=Route,
                                  handler_state=HState}=Resrc,
                     method=Method, log_data=LogData}) ->
    {ok, State2} =
        try Handler:init(Route, Req, HState) of
            {ok, HState1} ->
                State1 = State#state{resrc=Resrc#resrc{handler_state=HState1}},
                handle_request(http_method(Method), Req, State1);
            Else ->
                reply(500, [], <<>>, Req),
                badmatch_error_info(Else, {Handler, init, 3}, Route, Req, State),
                {ok, State#state{terminate_reason={error, badmatch}}}

        catch Class:Reason ->
                reply(500, [], <<>>, Req),
                error_info(Class, Reason, Route, Req, HState),
                {ok, State#state{terminate_reason={error, Reason}}}
        end,

    LogData1 = LogData#log_data{response_time = erlang:localtime()},
    receive
        {Status, ContentLength} ->
            {IP, _} = leptus_req:peer(Req),
            Version = leptus_req:version(Req),
            URI = leptus_req:uri(Req),
            LogData2 = LogData1#log_data{ip = IP, version = Version,
                                         uri = URI, status = Status,
                                         content_length = ContentLength},
            spawn(leptus_logger, send_event, [access_log, LogData2]),
            spawn(leptus_logger, send_event, [debug_log, LogData2])
    end,

    TerminateReason = State2#state.terminate_reason,
    HState2 = State2#state.resrc#resrc.handler_state,
    handler_terminate(TerminateReason, Handler, Route, Req, HState2),
    Req1 = leptus_req:get_req(Req),
    leptus_req:stop(Req),
    {ok, Req1, Env}.

%% -----------------------------------------------------------------------------
%% internal
%% -----------------------------------------------------------------------------
-spec is_defined(module(), atom()) -> boolean().
is_defined(Handler, Func) ->
    erlang:function_exported(Handler, Func, 3).

-spec http_method(binary()) -> method() | options | not_allowed.
http_method(<<"GET">>) -> get;
http_method(<<"PUT">>) -> put;
http_method(<<"POST">>) -> post;
http_method(<<"DELETE">>) -> delete;
%% just to deal with CORS preflight request
http_method(<<"OPTIONS">>) -> options;
http_method(_) -> not_allowed.

%% -----------------------------------------------------------------------------
%% Handler:Method/3 (Method :: get | put | post | delete)
%% -----------------------------------------------------------------------------
-spec handle_request(not_allowed | options | method(), req(), State) ->
                            {ok, State} when State :: state().
handle_request(not_allowed, Req,
               State=#state{resrc=#resrc{handler_state=HandlerState,
                                         handler=Handler, route=Route}}) ->
    Response = method_not_allowed(Handler, Route, HandlerState),
    handle_response(Response, Req, State#state{terminate_reason=not_allowed});
handle_request(options, Req, State=#state{
				      resrc=#resrc{
					       handler_state=HandlerState}}) ->
    %% deal with CORS preflight request
    handle_options_request(Req, State, HandlerState, check_cors_preflight(Req, State));
handle_request(Func, Req,
               State=#state{resrc=#resrc{handler=Handler, route=Route,
                                         handler_state=HandlerState},
                            method=Method}) ->
    %% reasponse and terminate reason
    {Response,
     TReason} = case is_allowed(Handler, Func, Route, Method) of
                    true ->
                        case authorization(Handler, Route, Req, HandlerState) of
                            {true, HandlerState1} ->
                                try Handler:Func(Route, Req, HandlerState1) of
                                    Resp ->
                                        {Resp, normal}
                                catch Class:Reason ->
                                        error_info(Class, Reason, Route, Req,
                                                   HandlerState1),
                                        {{500, <<>>, HandlerState1},
                                         {error, Reason}}
                                end;
                            {false, Resp, TR} ->
                                {Resp, TR}
                        end;
                    false ->
                        {method_not_allowed(Handler, Route, HandlerState),
                         not_allowed}
                end,
    handle_response(Response, Req, State#state{terminate_reason=TReason}).

check_cors_preflight(Req, #state{
			     resrc=#resrc{
				      handler=Handler,
				      route=Route}}) ->
    Method = leptus_req:header(Req, <<"access-control-request-method">>),
    is_allowed(Handler, http_method(Method), Route, Method).

handle_options_request(Req, State, HandlerState, true) ->
    handle_response({<<>>, HandlerState}, Req, State);
handle_options_request(Req, State, _, false) ->
    handle_request(not_allowed, Req, State).

%% -----------------------------------------------------------------------------
%% Handler:is_authenticated/3 and Handler:has_permission/3
%% -----------------------------------------------------------------------------
-spec authorization(handler(), route(), req(), handler_state()) ->
                           {true, handler_state()} |
                           {false, response(), terminate_reason()}.
authorization(Handler, Route, Req, HandlerState) ->
    %%
    %% spec:
    %%   is_authenticated(Route, Req, State) ->
    %%     {true, State} | {false, Body, State} | {false, Headers, Body, State}.
    %%
    F1 = is_authenticated,
    TR1 = unauthenticated, %% terminate reason
    Res = case is_defined(Handler, F1) of
              true ->
                  try Handler:F1(Route, Req, HandlerState) of
                      {true, HandlerState1} ->
                          {true, HandlerState1};
                      {false, Body, HandlerState1} ->
                          {false, {401, Body, HandlerState1}, TR1};
                      {false, Headers, Body, HandlerState1} ->
                          {false, {401, Headers, Body, HandlerState1}, TR1};
                      Else ->
                          badmatch_error_info(Else, {Handler, F1, 3}, Route,
                                              Req, HandlerState),
                          {false, {500, <<>>, HandlerState}, badmatch}

                  catch Class:Reason ->
                          error_info(Class, Reason, Route, Req, HandlerState),
                          {false, {500, <<>>, HandlerState}, {error, Reason}}
                  end;
              false ->
                  {true, HandlerState}
          end,

    %%
    %% spec:
    %%   has_permission(Route, Req, State) ->
    %%     {true, State} | {false, Body, State} | {false, Headers, Body, State}.
    %%
    F2 = has_permission,
    TR2 = no_permission, %% terminate reason
    case Res of
        {false, _, _} ->
            Res;
        {true, HandlerState2} ->
            case is_defined(Handler, F2) of
                true ->
                    try Handler:F2(Route, Req, HandlerState2) of
                        {true, HandlerState3} ->
                            {true, HandlerState3};
                        {false, Body1, HandlerState3} ->
                            {false, {403, Body1, HandlerState3}, TR2};
                        {false, Headers1, Body1, HandlerState3} ->
                            {false, {403, Headers1, Body1, HandlerState3}, TR2};
                        Else1 ->
                            badmatch_error_info(Else1, {Handler, F2, 3}, Route,
                                                Req, HandlerState2),
                            {false, {500, <<>>, HandlerState2}, badmatch}

                    catch Class1:Reason1 ->
                            error_info(Class1, Reason1, Route, Req,
                                       HandlerState2),
                            {false, {500, <<>>, HandlerState2}, {error, Reason1}}
                    end;
                false ->
                    {true, HandlerState2}
            end
    end.

%% -----------------------------------------------------------------------------
%% Handler:allowed_methods/1
%% check if method allowed
%% -----------------------------------------------------------------------------
-spec is_allowed(handler(), method(), route(), binary()) -> boolean().
is_allowed(Handler, Func, Route, Method) ->
    %% check if Handler:Func/3 is exported
    case is_defined(Handler, Func) of
        true ->
            %% check if the http method is existing in allowed methods list
            %%
            %% e.g.
            %%   lists:member(<<"GET">>, [<<"GET">>, <<"DELETE">>])
            %%
            lists:member(Method, Handler:allowed_methods(Route));
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
    {405, [{<<"allow">>, allowed_methods(Handler, Route)}], <<>>, HandlerState}.

-spec allowed_methods(handler(), route()) -> binary().
allowed_methods(Handler, Route) ->
    join_http_methods(Handler:allowed_methods(Route)).

%% -----------------------------------------------------------------------------
%% Handler:cross_domains/3
%% -----------------------------------------------------------------------------
-spec handler_cross_domains(handler(), route(), req(), handler_state()) ->
                                   {headers(), handler_state()}.
handler_cross_domains(Handler, Route, Req, HandlerState) ->
    %%
    %% spec:
    %%   Handler:cross_domains(Route, Req, State) -> {[string()], State}
    %%
    case leptus_req:header(Req, <<"origin">>) of
        undefined ->
            {[], HandlerState};
        Origin ->
            %% go on if the Origin header is present
            case is_defined(Handler, cross_domains) of
                false ->
                    {[], HandlerState};
                true ->
                    %% go on if Handler:cross_domains/3 is exported
                    F = cross_domains,
                    try Handler:F(Route, Req, HandlerState) of
                        {HostMatches, HandlerState1} ->
                            Host = leptus_utils:get_uri_authority(Origin),
                            case origin_matches(Host, HostMatches) of
                                false ->
                                    {[], HandlerState1};
                                %% go on if Origin is allowed
                                true ->
                                    {cors_headers(Handler, Route, Origin, Req),
                                     HandlerState1}
                            end;
                        Else ->
                            badmatch_error_info(Else, {Handler, F, 3}, Route,
                                                Req, HandlerState),
                            throw(badmatch)

                    catch Class:Reason ->
                            error_info(Class, Reason, Route, Req, HandlerState),
                            throw(Reason)
                    end
            end
    end.

-spec is_preflight(req()) -> boolean().
is_preflight(Req) ->
    case leptus_req:header(Req, <<"access-control-request-method">>) of
        undefined -> false;
        _ -> true
    end.

-spec cors_headers(handler(), route(), binary(), req()) -> headers().
cors_headers(Handler, Route, Origin, Req) ->
    AccessControlAllowOrigin = {<<"access-control-allow-origin">>, Origin},
    case is_preflight(Req) of
        true ->
            [AccessControlAllowOrigin|[{<<"access-control-allow-methods">>,
                                        allowed_methods(Handler, Route)}]];
        false ->
            [AccessControlAllowOrigin]
    end.

%% -----------------------------------------------------------------------------
%% Handler:terminate/4
%% -----------------------------------------------------------------------------
-spec handler_terminate(terminate_reason(), handler(), route(), req(),
                        handler_state()) -> ok.
handler_terminate(Reason, Handler, Route, Req, HandlerState) ->
    Handler:terminate(Reason, Route, Req, HandlerState).

%% -----------------------------------------------------------------------------
%% reply - prepare stauts, headers and body
%% -----------------------------------------------------------------------------
-spec handle_response(response(), req(), State) ->
                             {ok, State} when State :: state().
handle_response({Body, HandlerState}, Req, St=#state{resrc=Resrc}) ->
    handle_response(200, [], Body, Req,
                    St#state{resrc=Resrc#resrc{handler_state = HandlerState}});
handle_response({Status, Body, HandlerState}, Req, St=#state{resrc=Resrc}) ->
    handle_response(Status, [], Body, Req,
                    St#state{resrc=Resrc#resrc{handler_state = HandlerState}});
handle_response({Status, Headers, Body, HandlerState}, Req,
                St=#state{resrc=Resrc}) ->
    handle_response(Status, Headers, Body, Req,
                    St#state{resrc=Resrc#resrc{handler_state = HandlerState}}).

-spec handle_response(status(), headers(), body(), req(), St) ->
                             {ok, St} when St :: state().
handle_response(Status, Headers, Body, Req,
                State=#state{terminate_reason={error, _}}) ->
    reply(Status, Headers, Body, Req),
    {ok, State};
handle_response(Status, Headers, Body, Req,
                State=#state{resrc=Resrc=#resrc{handler=Handler,route=Route,
                                                handler_state=HandlerState}}) ->
    Status1 = status(Status),
    %% encode Body and set content-type
    {Headers1, Body1} = prepare_headers_body(Headers, Body),

    %% enable or disable cross-domain requests
    try handler_cross_domains(Handler, Route, Req, HandlerState) of
        {Headers2, HandlerState1} ->
            Headers3 = Headers1 ++ Headers2,
            reply(Status1, Headers3, Body1, Req),
            {ok, State#state{resrc=Resrc#resrc{handler_state = HandlerState1}}}
    catch _:Reason ->
            reply(500, [], <<>>, Req),
            {ok, State#state{terminate_reason={error, Reason}}}
    end.

-spec reply(status(), headers(), body(), req()) -> ok.
reply(Status, Headers, Body, Req) ->
    %% used in upgrade/4 for logging purposes
    self() ! {Status, iolist_size(Body)},
    leptus_req:reply(Req, Status, Headers, Body).

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

badmatch_error_info(Value, MFA, Route, Req, State) ->
    error_logger:error_msg("** Leptus handler terminating~n"
                           "** Bad return value in ~p~n"
                           "** Route == ~p~n"
                           "** Req == ~p~n"
                           "** Handler state == ~p~n"
                           "** Return value == ~p~n",
                           [MFA, Route, Req, State, Value]).

error_info(Class, Reason, Route, Req, State) ->
    error_logger:error_msg("** Leptus handler terminating~n"
                           "** Exception class ~p in process ~p~n"
                           "** Route == ~p~n"
                           "** Req == ~p~n"
                           "** Handler state == ~p~n"
                           "** Reason for termination ==~n"
                           "** ~p~n",
                           [Class, self(), Route, Req, State,
                            {Reason, erlang:get_stacktrace()}]).
