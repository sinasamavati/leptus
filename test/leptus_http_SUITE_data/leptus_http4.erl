-module(leptus_http4).

-export([routes/0]).
-export([allowed_methods/1]).
-export([init/3]).
-export([get/3]).
-export([terminate/3]).

routes() -> ["/msgpack/:msg"].
allowed_methods("/msgpack/:msg") -> <<"GET">>.
init(_Route, _Req, State) -> {ok, State}.

get("/msgpack/:msg", Req, State) ->
    Msg = leptus_req:param(msg, Req),
    {{msgpack, [{<<"msg">>, Msg}]}, State}.

terminate(_Reason, _Req, _State) ->
    ok.
