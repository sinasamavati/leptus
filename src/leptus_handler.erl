-module(leptus_handler).

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
-type state() :: any().

-record(state, {handler, route, handler_state}).


init(_Transport, Req, State) ->
    Handler = get_handler(State),
    Route = get_route(State),
    case handler_init(Handler, Route, Req) of
        {ok, HandlerState} ->
            State1 = set_handler_state(State, HandlerState),
            {ok, Req, State1};
        Else ->
            Else
    end.

handle(Req, State) ->
    Handler = get_handler(State),
    Route = get_route(State),

    %% convert the http method to a lowercase atom
    Func = http_method(leptus_req:method(Req)),
    handle_request(Handler, Func, Route, Req, State).

terminate(_Reason, _Req, _State) ->
    %% TODO: Handler:terminate/3
    ok.


%% internal
get_handler(State) ->
    State#state.handler.

get_route(State) ->
    State#state.route.

get_handler_state(State) ->
    State#state.handler_state.

set_handler_state(State, HandlerState) ->
    State#state{handler_state=HandlerState}.

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

-spec handler_init(handler(), route(), req()) -> {ok, state()}.
handler_init(Handler, Route, Req) ->
    Handler:init(Route, Req, []).

handle_request(Handler, Func, Route, Req, State) ->
    Response = case is_defined(Handler, Func) of
                   true ->
                       case handler_is_authorized(Handler, Route, Req, State) of
                           {true, State1} ->
                               %% method not allowed if function doesn't match
                               try
                                   Handler:Func(Route, Req, State1)
                               catch
                                   error:function_clause ->
                                       method_not_allowed(Handler, Route)
                               end;
                           {false, Res} ->
                               Res
                       end;
                   false ->
                       %% method not allowed if function is not exported
                       method_not_allowed(Handler, Route)
               end,
    reply(Response, Req).

-spec handler_is_authorized(handler(), route(), req(), state()) ->
                                   {true, state()}
                                       | {false, {401, body(), state()}}
                                       | {false, {401, headers(), body(), state()}}.
handler_is_authorized(Handler, Route, Req, State) ->
    %% spec: is_authorized(Route, State, Req) ->
    %%           {true, State} | {false, Body, State} | {false, Headers, Body, State}.
    case is_defined(Handler, is_authorized) of
        true ->
            HandlerState = get_handler_state(State),
            case Handler:is_authorized(Route, Req, HandlerState) of
                {true, State1} ->
                    {true, State1};
                {false, Body, State1} ->
                    {false, {401, Body, State1}};
                {false, Headers, Body, State1} ->
                    {false, {401, Headers, Body, State1}}
            end;
        false ->
            {true, State}
    end.

method_not_allowed(Handler, Route) ->
    Methods = Handler:allowed_methods(Route),
    <<", ", Allow/binary>> = << <<", ", M/binary>> || M <- Methods >>,
    {405, [{<<"Allow">>, Allow}], <<>>}.

-spec reply({body(), state()}
            | {status(), body(), state()}
            | {status(), headers(), body(), state()}, req()) ->
                   {ok, req(), state()}.
reply({Body, State}, Req) ->
    reply(200, [], Body, Req, State);
reply({Status, Body, State}, Req) ->
    reply(Status, [], Body, Req, State);
reply({Status, Headers, Body, State}, Req) ->
    reply(Status, Headers, Body, Req, State).

reply(Status, Headers, Body, Req, State) ->
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
    {ok, Req1, State}.
