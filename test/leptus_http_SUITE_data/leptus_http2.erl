-module(leptus_http2).

%% leptus callbacks
-export([routes/0]).
-export([init/3]).
-export([is_authorized/3]).
-export([allowed_methods/1]).
-export([get/3]).
-export([put/3]).
-export([post/3]).
-export([terminate/3]).


routes() ->
    ["/users/:id", "/users/:id/interests", "/users/:id/profile"].

init(_Route, _Req, _State) ->
    {ok, blah}.

allowed_methods("/users/:id") -> <<"GET, PUT, POST">>;
allowed_methods("/users/:id/interests") -> <<"GET">>;
allowed_methods("/users/:id/profile") -> <<"GET">>.

is_authorized("/users/:id", Req, State) ->
    case leptus_req:method(Req) of
        <<"PUT">> ->
            check_auth(Req, State);
        <<"POST">> ->
            case check_auth(Req, State) of
                {false, _, _} ->
                    {false, {json, [{<<"error">>, <<"unauthorized">>}]}, State};
                Else ->
                    Else
            end;
        _ ->
            {true, State}
    end;
is_authorized(_Route, _Req, State) ->
    {true, State}.

get("/users/:id", Req, State) ->
    Id = leptus_req:param(id, Req),
    {["aha, this is ", Id], State};

get("/users/:id/interests", Req, State) ->
    Id = leptus_req:param(id, Req),
    case Id of
        <<"s1n4">> ->
            {200, <<"Erlang and a lotta things else">>, State};
        <<"456">> ->
            {"art, photography...", State};
        _ ->
            {404, <<"not found...">>, State}
    end;

get("/users/:id/profile", Req, State) ->
    Body = [
            {<<"id">>, leptus_req:param(id, Req)},
            {<<"bio">>, <<"Erlanger">>},
            {<<"github">>, leptus_req:param(id, Req)}
           ],
    {200, {json, Body}, State}.

put("/users/:id", _Req, State) ->
    {200, <<"updated">>, State}.

post("/users/:id", _Req, State) ->
    {200, <<"updated">>, State}.


%% internal
check_auth(Req, State) ->
    case leptus_req:auth(<<"basic">>, Req) of
        {<<"sina">>, <<"wrote_me">>} ->
            {true, State};
        _ ->
            {false, <<"unauthorized.">>, State}
    end.

terminate(_Reason, _Req, State) ->
    blah = State,
    ok.
