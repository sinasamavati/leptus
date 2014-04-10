# Callbacks

There are three callbacks which are required for every request handler: `init/3`,
`HttpMethod/3` and `terminate/4`.

* [prefix/0](#prefix0)
* [init/3](#init3)
* [cross_domains/3](#cross_domains3)
* [is_authorized/3](#is_authorized3)
* [HttpMethod/3](#httpmethod3)
* [terminate/4](#terminate4)

#### prefix/0

This is an optional callback which you can use for prefixing routes.

```erlang
Module:prefix() -> string()
```

Example:
```erlang
prefix() -> "/v1".
```

NOTE: this won't affect `Route`s in the handler, but instead, this will be used
when gathering routes and starting the Cowboy listener.

#### init/3

```erlang
Module:init(Route, Req, State) ->
    {ok, State}.
```

#### cross_domains/3

This is an optional callback that lets you enable cross-domain requests
([CORS](http://en.wikipedia.org/wiki/Cross-origin_resource_sharing)).

```erlang
Module:cross_domains(Route, Req, State) -> [HostMatch]
```

`HostMatch` is equal to Cowboy HostMatch syntax.

This will be used when preparing headers right before replying.

If one of the HostMatches and Origin match, `access-control-allow-origin` will
be set to Origin.

#### is_authorized/3

Exporting this callback in a module means that every request that should come to
the handler needs authorization.

```erlang
Module:is_authorized(Route, Req, State) ->
    {true, State} | {false, Body, State} | {false, Headers, Body, State}
```

#### HttpMethod/3

This means `get/3`, `put/3`, `post/3`, `delete/3`.

```erlang
Module:HttpMethod(Route, Req, State) ->
    {Body, State} | {Status, Body, State} | {Status, Headers, Body, State}
```

In this case, `Route` must be a pattern matching.

Examples:

```erlang
get("/", Req, State) ->
    ...
    {<<"index">>, State}.

put("/:id/edit", Req, State) ->
    ...
    {200, <<"edited">>, State}.

post("/new", Req, State) ->
    ...
    {201, [{<<"Location">>, <<"/data/386">>}], <<"created">>, State}.

delete("/:id", Req, State) ->
    ...
    %% Body as a json
    {204, {json, [{<<"message">>, <<"deleted">>}]}, State}.
```

#### terminate/4

```erlang
Module:terminate(Reason, Route, Req, State) -> ok
```

## Example

Please pay attention to comment.

```erlang
-module(example).
-compile({parse_transform, leptus_pt}).

-export([prefix/0]).
-export([init/3]).
-export([cross_domains/3]).
-export([is_authorized/3]).
-export([get/3]).
-export([terminate/4]).

prefix() -> "/example".

init(_Route, _Req, State) ->
    {ok, State}.

cross_domains(_Route, _Req, _State) ->
    ['_'].

is_authorized(_Route, _Req, State) ->
    {true, State}.

%% Route is "/1" in every callback in this example,
%% but we used prefix/0 to prepend "/example",
%% so this will be used by issuing the url '/example/1'
get("/1", _Req, State) ->
    {<<"Example 1!">>, State}.

terminate(_Reason, _Route, _Req, _State) ->
    ok.
```
