-module(leptus_http2).

-export([routes/0]).
-export([get/2]).
-export([put/2]).
-export([post/2]).
-export([is_authorized/3]).


routes() ->
    ["/users/:id", "/users/:id/interests", "/users/:id/profile"].

is_authorized(<<"PUT">>, "/users/:id", Req) ->
    check_auth(Req);
is_authorized(<<"POST">>, "/users/:id", Req) ->
    case check_auth(Req) of
        {false, _} ->
            {false, json, [{<<"error">>, <<"unauthorized">>}]};
        _ ->
            true
    end;
is_authorized(_, _, _Req) ->
    true.

get("/users/:id", Req) ->
    Id = leptus_req:binding(id, Req),
    {200, ["aha, this is ", Id]};

get("/users/:id/interests", Req) ->
    Id = leptus_req:binding(id, Req),
    case Id of
        <<"s1n4">> ->
            {200, <<"Erlang and a lotta things else">>};
        <<"456">> ->
            {200, "art, photography..."};
        _ ->
            {404, <<"not found...">>}
    end;

get("/users/:id/profile", Req) ->
    Body = [
            {<<"id">>, leptus_req:binding(id, Req)},
            {<<"bio">>, <<"Erlanger">>},
            {<<"github">>, leptus_req:binding(id, Req)}
           ],
    {200, json, Body}.

put("/users/:id", _Req) ->
    {200, <<"updated">>}.

post("/users/:id", _Req) ->
    {200, <<"updated">>}.


%% internal
check_auth(Req) ->
    case leptus_req:auth(<<"basic">>, Req) of
        {<<"sina">>, <<"wrote_me">>} ->
            true;
        _ ->
            {false, <<"unauthorized.">>}
    end.
