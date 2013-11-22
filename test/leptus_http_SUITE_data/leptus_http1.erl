-module(leptus_http1).

%% leptus callbacks
-export([routes/0]).
-export([init/3]).
-export([get/3]).
-export([terminate/3]).

routes() ->
    ["/", "/hello", "/hello/:name"].

init(_Route, _Req, State) ->
    {ok, State}.

get("/", _Req, State) ->
    {"index", State};
get("/hello", _Req, State) ->
    {200, <<"hello, world!">>, State};
get("/hello/:name", Req, State) ->
    Name = leptus_req:param(name, Req),
    {200, "hello, " ++ Name, State}.

terminate(_Reason, _Req, _State) ->
    ok.
