-module(leptus_handler).
-author("Sina Samavati <sina.samv@gmail.com>").

%% cowboy callbacks
-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-type handler() :: module().
-type req() :: cowboy_req:req().
-type route() :: cowboy_router:route_match().
-type status() :: non_neg_integer() | binary().
-type headers() :: json | cowboy:http_headers().
-type body() :: binary() | string().
-type handler_state() :: any().
-type method() :: get | put | post | delete.

-record(ctx, {
          handler :: module(),
          route :: route(),
          handler_state :: handler_state()
         }).
-type ctx() :: #ctx{}.


init(_Transport, Req, Ctx) ->
    Handler = get_handler(Ctx),
    Route = get_route(Ctx),
    HandlerState = get_handler_state(Ctx),
    case handler_init(Handler, Route, Req, HandlerState) of
        {ok, HandlerState1} ->
            Ctx1 = set_handler_state(Ctx, HandlerState1),
            {ok, Req, Ctx1};
        Else ->
            Else
    end.

handle(Req, Ctx) ->
    Handler = get_handler(Ctx),
    Route = get_route(Ctx),

    %% convert the http method to a lowercase atom
    Func = http_method(leptus_req:method(Req)),
    handle_request(Handler, Func, Route, Req, Ctx).

terminate(Reason, Req, Ctx) ->
    handler_terminate(Reason, Req, Ctx).


%% internal
get_handler(Ctx) ->
    Ctx#ctx.handler.

get_route(Ctx) ->
    Ctx#ctx.route.

get_handler_state(Ctx) ->
    Ctx#ctx.handler_state.

set_handler_state(Ctx, HandlerState) ->
    Ctx#ctx{handler_state=HandlerState}.

is_defined(Handler, Func) ->
    erlang:function_exported(Handler, Func, 3).

-spec http_method(binary()) -> atom().
http_method(<<"GET">>) -> get;
http_method(<<"PUT">>) -> put;
http_method(<<"POST">>) -> post;
http_method(<<"DELETE">>) -> delete;
http_method(Method) ->
    %% TODO: decide to change or remove it
    list_to_atom([M - $A + $a || <<M>>  <= Method]).

-spec handler_init(handler(), route(), req(), handler_state()) ->
                          {ok, handler_state()}.
handler_init(Handler, Route, Req, HandlerState) ->
    Handler:init(Route, Req, HandlerState).

-spec handle_request(handler(), method(), route(), req(), ctx()) ->
                            {ok, req(), ctx()}.
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

-spec handler_is_authorized(handler(), route(), req(), handler_state()) ->
                                   {true, handler_state()}
                                       | {false, {401, body(), handler_state()}}
                                       | {false, {401, headers(), body(), handler_state()}}.
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

method_not_allowed(Handler, Route, HandlerState) ->
    Methods = Handler:allowed_methods(Route),
    {405, [{<<"Allow">>, Methods}], <<>>, HandlerState}.

-spec reply({body(), handler_state()}
            | {status(), body(), handler_state()}
            | {status(), headers(), body(), handler_state()}, req(), ctx()) ->
                   {ok, req(), ctx()}.
reply({Body, HandlerState}, Req, Ctx) ->
    reply(200, [], Body, HandlerState, Req, Ctx);
reply({Status, Body, HandlerState}, Req, Ctx) ->
    reply(Status, [], Body, HandlerState, Req, Ctx);
reply({Status, Headers, Body, HandlerState}, Req, Ctx) ->
    reply(Status, Headers, Body, HandlerState, Req, Ctx).

reply(Status, Headers, Body, HandlerState, Req, Ctx) ->
    {
      Headers1,
      Body1
    } = case Headers of
            json ->
                {[{<<"content-type">>, <<"application/json">>}],
                 jiffy:encode({Body})};
            [] ->
                {[{<<"content-type">>, <<"text/plain">>}], Body};
            _ ->
                {Headers, Body}
        end,
    {ok, Req1} = cowboy_req:reply(Status, Headers1, Body1, Req),
    {ok, Req1, set_handler_state(Ctx, HandlerState)}.

handler_terminate(Reason, Req, Ctx) ->
    Handler = get_handler(Ctx),
    HandlerState = get_handler_state(Ctx),
    Handler:terminate(Reason, Req, HandlerState).
