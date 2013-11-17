-module(leptus_resouce_handler).
-author("Sina Samavati <sina.samv@gmail.com>").

%% cowboy callbacks
-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-type method() :: binary().
-type req() :: cowboy_req:req().
-type route() :: cowboy_router:route_match().


init(_Transport, Req, State) ->
    %% TODO: Handler:init/3
    {ok, Req, State}.

handle(Req, State) ->
    Method = leptus_req:method(Req),
    handle_request(Method, Req, State).

terminate(_Reason, _Req, _State) ->
    %% TODO: Handler:terminate/3
    ok.


%% internal
-spec http_method(method()) -> atom().
http_method(<<"GET">>) -> get;
http_method(<<"PUT">>) -> put;
http_method(<<"POST">>) -> post;
http_method(<<"DELETE">>) -> delete;
http_method(Method) ->
    %% TODO: decide to change or remove it
    list_to_atom([M - $A + $a || <<M>>  <= Method]).

%% check if request handler is exported
-spec is_rqh_exported(module(), function()) -> boolean().
is_rqh_exported(Handler, Func) ->
    erlang:function_exported(Handler, Func, 2).

%% the heart of leptus
-spec handle_request(method(), {module(), req()}, route()) ->
                            {ok, req(), route()}.
handle_request(Method, Req, State={Handler, Route}) ->
    %% convert the http method to a lowercase atom
    Func = http_method(Method),

    %% method not allowed if function is not exported
    Args = case is_rqh_exported(Handler, Func) of
               true ->
                   %% handle authorization
                   case handle_authorization(Handler, Route, Req) of
                       true ->
                           %% method not allowed if function doesn't match
                           try
                               Handler:Func(Route, Req)
                           catch
                               %% TODO: find an alternative way
                               error:function_clause ->
                                   method_not_allowed(State)
                           end;
                       {false, Args1} ->
                           Args1
                   end;

               false ->
                   method_not_allowed(State)
           end,
    reply(Args, Req, State).

-spec handle_authorization(module(), route(), req()) ->
                                  true | {false, {401, term()}} |
                                  {false, {401, json, term()}}.
handle_authorization(Handler, State, Req) ->
    %% spec: is_authorized(State, Req) ->
    %%           true | {false, Body} | {false, json, JsonTerm}.
    case erlang:function_exported(Handler, is_authorized, 2) of
        true ->
            case Handler:is_authorized(State, Req) of
                true ->
                    true;
                {false, Body} ->
                    {false, {401, Body}};
                {false, json, Body} ->
                    {false, {401, json, Body}}
            end;
        false ->
            true
    end.

reply({Status, Body}, Req, State) ->
    reply(Status, [], Body, Req, State);
reply({Status, Headers, Body}, Req, State) ->
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

method_not_allowed({Handler, Route}) ->
    Methods = Handler:allowed_methods(Route),
    <<", ", Allow/binary>> = << <<", ", M/binary>> || M <- Methods >>,
    {405, [{<<"Allow">>, Allow}], <<>>}.
