# Callbacks

There are three callbacks which are required for every request handler: `init/3`, `HttpMethod/3` and `terminate/3`.

* [init/3](#init3)
* [is_authorized/3](#isauthorized_3)
* [HttpMethod/3](#httpmethod3)
* [terminate/3](#terminate3)

#### init/3

```erlang
Module:init(Route, Req, State) ->
    {ok, State}.
```

#### is_authorized/3

Exporting this callback in a module means that every request that should come to the request handler needs authorization.

```erlang
Module:is_authorized(Route, Req, State) ->
    {true, State} | {false, Body, State} | {false, Headers, Body, State}.
```

#### HttpMethod/3

This means `get/3`, `put/3`, `post/3`, `delete/3`.

```erlang
Module:HttpMethod(Route, Req, State) ->
    {Body, State} | {Status, Body, State} | {Status, Headers, Body, State}.
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

#### terminate/3

```erlang
Module:terminate(Reason, Req, State) ->
    ok.
```
