-module(leptus_http3).

-export([routes/0]).
-export([post/2]).


routes() ->
    ["/user/register"].

post("/user/register", Req) ->
    Body = leptus_req:body_qs(Req),
    Username = proplists:get_value(<<"username">>, Body),
    case Username of
        <<"asdf">> ->
            {403, <<"Username is already taken.">>};
        _ ->
            {201, "Thanks for registration."}
    end.
