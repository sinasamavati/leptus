-module(leptus_http2).

-export([routes/0]).
-export([get/2]).

routes() ->
    ["/users/:id", "/users/:id/interests"].

get("/users/:id", Req) ->
    {Id, _} = cowboy_req:binding(id, Req),
    {ok, 200, ["aha, this is ", Id]};

get("/users/:id/interests", Req) ->
    {Id, _} = cowboy_req:binding(id, Req),
    case Id of
        <<"s1n4">> ->
            {ok, 200, "Erlang and a lotta things else"};
        <<"456">> ->
            {ok, 404, "not found"};
        _ ->
            {ok, 200, "art, photography..."}
    end.
