-module(leptus_http3).

%% leptus callbacks
-export([routes/0]).
-export([init/3]).
-export([allowed_methods/1]).
-export([post/3]).
-export([put/3]).
-export([delete/3]).
-export([terminate/3]).


routes() ->
    ["/user/register", "/settings/change-password", "/users/:username/posts/:id"].

init(_Route, _Req, _State) ->
    {ok, my_state}.

allowed_methods("/user/register") -> [<<"POST">>];
allowed_methods("/settings/change-password") -> [<<"PUT">>];
allowed_methods(_) -> [<<"DELETE">>].

post("/user/register", Req, State) ->
    Body = leptus_req:body_qs(Req),
    Username = proplists:get_value(<<"username">>, Body),
    case Username of
        <<"asdf">> ->
            {403, <<"Username is already taken.">>, State};
        _ ->
            {201, <<"Thanks for registration.">>, State}
    end.

put("/settings/change-password", Req, State) ->
    [
     {<<"password">>, P1}, {<<"password_confirmation">>, P2}
    ] = leptus_req:body_qs(Req),

    if P1 =:= P2 ->
            {<<"Your password has been changed.">>, State};
       true ->
            {403, <<"Passwords didn't match.">>, badmatch}
    end.

delete("/users/:username/posts/:id", Req, State) ->
    my_state = State,
    IdLen = byte_size(leptus_req:binding(id, Req)),
    if IdLen >= 4 ->
            {404, <<>>, dammit};

       true ->
            {204, <<>>, aha}
    end.

terminate(_Reason, _Req, State) ->
    case State of
        my_state -> ok;
        badmatch -> ok;
        aha -> ok;
        dammit -> ok
    end.
