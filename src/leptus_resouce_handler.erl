-module(leptus_resouce_handler).
-author("Sina Samavati <sina.samv@gmail.com>").

%% cowboy callbacks
-export([init/3]).
-export([handle/2]).
-export([terminate/3]).


init(_Transport, Req, State) ->
    {ok, Req, State}.

handle(Req, State) ->
    {Method, _} = cowboy_req:method(Req),
    handle_request(Method, Req, State).

terminate(_Reason, _Req, _State) ->
    ok.


%% internal

%% the heart of leptus
handle_request(Method, Req, State) ->
    Args = case leptus_router:find_mod(State) of
               {ok, Mod} ->
                   %% convert the http method to a lowercase atom
                   Func = list_to_atom([M - $A + $a || <<M>>  <= Method]),

                   %% method not allowed if function is not exported
                   case erlang:function_exported(Mod, Func, 2) of
                       true ->
                           %% handle authorization
                           case handle_authorization(Mod, Method, State, Req) of
                               true ->
                                   %% method not allowed if function doesn't match
                                   try
                                       apply(Mod, Func, [State, Req])
                                   catch
                                       %% TODO: find an alternative way
                                       error:function_clause -> {405, <<>>}
                                   end;
                               {false, Args1} ->
                                   Args1
                           end;

                       false ->
                           {405, <<>>}
                   end;

               {error, undefined} ->
                   {404, <<>>}
           end,
    reply(Args, Req, State).


handle_authorization(Mod, Method, State, Req) ->
    %% spec: is_authorized(Method, State, Req) ->
    %%           true | {false, Body} | {false, json, JsonTerm}.
    case erlang:function_exported(Mod, is_authorized, 3) of
        true ->
            case apply(Mod, is_authorized, [Method, State, Req]) of
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
                 jiffy:encode(Body)};
            [] ->
                {[{<<"content-type">>, <<"text/plain">>}], Body};
            _ ->
                {Headers, Body}
        end,
    {ok, Req1} = cowboy_req:reply(Status, Headers1, Body1, Req),
    {ok, Req1, State}.
