-module(leptus_resouce_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).


init(_Transport, Req, State) ->
    {ok, Req, State}.

handle(Req, State) ->
    {Method, Req2} = cowboy_req:method(Req),
    {ok, Status, Resp} = handle_request(Method, Req2, State),
    Req3 = cowboy_req:reply(Status, [], Resp, Req2),
    {ok, Req3, State}.

terminate(_Reason, _Req, _State) ->
    ok.


%% internal
handle_request(Method, Req, State) ->
    %% convert request method to an lowercase atom & apply the function
    case leptus_router:find_mod(State) of
        {ok, Mod} ->
            apply(Mod, list_to_atom([M - $A + $a || <<M>>  <= Method]),
                  [State, Req]);
        {error, undefined} ->
            {ok, 404, "not found"}
    end.
