%% This file is part of leptus, and released under the MIT license.
%% See LICENSE for more information.

-module(leptus_handler).
-author("Sina Samavati <sina.samv@gmail.com>").

%% cowboy callbacks
-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-type handler() :: module().
-type route() :: cowboy_router:route_match().
-type status() :: non_neg_integer() | binary().
-type headers() :: cowboy:http_headers().
-type body() :: binary() | string() | {json | msgpack, json_term()}.
-type handler_state() :: any().
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
-type data_format() :: text | json | msgpack.

-include("leptus.hrl").


-spec init({module(), http}, Req, Ctx) ->
                  {ok, Req, Ctx} when Req::cowboy_req:req(), Ctx::ctx().
init(_Transport, Req, Ctx) ->
    Handler = get_handler(Ctx),
    Route = get_route(Ctx),
    HandlerState = get_handler_state(Ctx),
    {ok, HandlerState1} = handler_init(Handler, Route, Req, HandlerState),
    {ok, Req, set_handler_state(Ctx, HandlerState1)}.


-spec handle(Req, Ctx) -> {ok, Req, Ctx} when Req::cowboy_req:req(), Ctx::ctx().
handle(Req, Ctx) ->
    Handler = get_handler(Ctx),
    Route = get_route(Ctx),

    %% convert the http method to a lowercase atom
    Func = http_method(leptus_req:method(Req)),
    handle_request(Handler, Func, Route, Req, Ctx).

-spec terminate(terminate_reason(), cowboy_req:req(), ctx()) -> ok.
terminate(Reason, Req, Ctx) ->
    handler_terminate(Reason, Req, Ctx).


%% internal
-spec get_handler(ctx()) -> handler().
get_handler(Ctx) ->
    Ctx#ctx.handler.

-spec get_route(ctx()) -> route().
get_route(Ctx) ->
    Ctx#ctx.route.

-spec get_handler_state(ctx()) -> any().
get_handler_state(Ctx) ->
    Ctx#ctx.handler_state.

-spec set_handler_state(Ctx, any()) -> Ctx when Ctx::ctx().
set_handler_state(Ctx, HandlerState) ->
    Ctx#ctx{handler_state=HandlerState}.

-spec is_defined(module(), atom()) -> boolean().
is_defined(Handler, Func) ->
    erlang:function_exported(Handler, Func, 3).

-spec http_method(binary()) -> method() | badarg.
http_method(<<"GET">>) -> get;
http_method(<<"PUT">>) -> put;
http_method(<<"POST">>) -> post;
http_method(<<"DELETE">>) -> delete;
http_method(_) -> badarg.

-spec handler_init(handler(), route(), Req, handler_state()) ->
                          {ok, handler_state()} when Req::cowboy_req:req().
handler_init(Handler, Route, Req, HandlerState) ->
    Handler:init(Route, Req, HandlerState).

-spec handle_request(handler(), method() | badarg, route(), Req, Ctx) ->
                            {ok, Req, Ctx} when Req::cowboy_req:req(),
                                                Ctx::ctx().
handle_request(Handler, badarg, Route, Req, Ctx) ->
    Response = method_not_allowed(Handler, Route, get_handler_state(Ctx)),
    reply(Response, Req, Ctx);
handle_request(Handler, Func, Route, Req, Ctx) ->
    HandlerState = get_handler_state(Ctx),
    Response = case is_defined(Handler, Func) of
                   true ->
                       case handler_is_authorized(Handler, Route, Req, HandlerState) of
                           {true, HandlerState1} ->
                               %% method not allowed if function doesn't match
                               try
                                   Handler:Func(Route, Req, HandlerState1)
                               catch
                                   error:function_clause ->
                                       method_not_allowed(Handler, Route, HandlerState)
                               end;
                           {false, Res} ->
                               Res
                       end;
                   false ->
                       %% method not allowed if function is not exported
                       method_not_allowed(Handler, Route, HandlerState)
               end,
    reply(Response, Req, Ctx).

-spec handler_is_authorized(handler(), route(), cowboy_req:req(), handler_state()) ->
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

-spec method_not_allowed(handler(), route(), handler_state()) -> response().
method_not_allowed(Handler, Route, HandlerState) ->
    %%
    %% spec:
    %%   allowed_methods(Route) -> binary()
    %% e.g.
    %%   allowed_methods("/") -> <<"GET, "POST">>
    %%
    {405, [{<<"allow">>, Handler:allowed_methods(Route)}], <<>>, HandlerState}.

-spec reply(response(), Req, Ctx) -> {ok, Req, Ctx} when Req::cowboy_req:req(),
                                                         Ctx::ctx().
reply({Body, HandlerState}, Req, Ctx) ->
    reply(200, [], Body, HandlerState, Req, Ctx);
reply({Status, Body, HandlerState}, Req, Ctx) ->
    reply(Status, [], Body, HandlerState, Req, Ctx);
reply({Status, Headers, Body, HandlerState}, Req, Ctx) ->
    reply(Status, Headers, Body, HandlerState, Req, Ctx).

-spec reply(status(), headers(), body(), handler_state(), Req, Ctx) ->
                   {ok, Req, Ctx} when Req::cowboy_req:req(), Ctx::ctx().
reply(Status, Headers, Body, HandlerState, Req, Ctx) ->
    {
      Headers1,
      Body1
    } = case Body of
            {json, Body2} ->
                {set_content_type(json, Headers), leptus_json:encode(Body2)};
            {msgpack, Body2}->
                {set_content_type(msgpack, Headers), msgpack:pack({Body2}, [jiffy])};
            _ ->
                {set_content_type(text, Headers), Body}
        end,
    {ok, Req1} = cowboy_req:reply(Status, Headers1, Body1, Req),
    {ok, Req1, set_handler_state(Ctx, HandlerState)}.

-spec handler_terminate(terminate_reason(), cowboy_req:req(), ctx()) -> ok.
handler_terminate(Reason, Req, Ctx) ->
    Handler = get_handler(Ctx),
    HandlerState = get_handler_state(Ctx),
    Handler:terminate(Reason, Req, HandlerState).

-spec set_content_type(data_format(), headers()) -> headers().
set_content_type(Type, Headers) ->
    [{<<"content-type">>, content_type(Type)}|Headers].

content_type(text) -> <<"text/plain">>;
content_type(json) -> <<"application/json">>;
content_type(msgpack) -> <<"application/x-msgpack">>.
