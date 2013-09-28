-module(leptus_resouce_handler).

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
handle_request(Method, Req, State) ->
    Args = case leptus_router:find_mod(State) of
               {ok, Mod} ->
                   %% convert the http method to a lowercase atom
                   Func = list_to_atom([M - $A + $a || <<M>>  <= Method]),

                   %% method not allowed if function is not exported
                   %% and/or doesn't match
                   case erlang:function_exported(Mod, Func, 2) of
                       true ->
                           try
                               apply(Mod, Func, [State, Req])
                           catch
                               error:function_clause -> {405, <<>>}
                           end;
                       false ->
                           {405, <<>>}
                   end;

               {error, undefined} ->
                   {404, <<>>}
           end,
    reply(Args, Req, State).

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
                 jsx:encode(Body)};
            [] ->
                {[{<<"content-type">>, <<"text/plain">>}], Body};
            _ ->
                {Headers, Body}
        end,
    {ok, Req1} = cowboy_req:reply(Status, Headers1, Body1, Req),
    {ok, Req1, State}.
