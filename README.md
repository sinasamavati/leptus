# Leptus [![Build Status](https://travis-ci.org/s1n4/leptus.png?branch=master)](https://travis-ci.org/s1n4/leptus)

Leptus is an Erlang REST framework that runs on top of cowboy.

Leptus aims at simply creating RESTful APIs with a straightforward principle which is:
**You get a request and you should give a response without manipulating the request object.**

## Requirements

  * Erlang/OTP R15B or newer
  * [cowboy](https://github.com/extend/cowboy)
  * [jiffy](https://github.com/davisp/jiffy) or [jsx](https://github.com/talentdeficit/jsx)
  * [msgpack](https://github.com/msgpack/msgpack-erlang)

## Installation

Clone it and just run `make`

OR

If you want to use it as a dependency in your project add the following to your rebar configuration

```
{deps, [
        ...
        {leptus, ".*", {git, "git://github.com/s1n4/leptus.git", {branch, "master"}}}
       ]}.
```

NOTE: if you prefer [jsx](https://github.com/talentdeficit/jsx) rather than [jiffy](https://github.com/davisp/jiffy)
the environment variable `USE_JSX` must be set to `true` when getting dependencies and/or compiling.

i.e.
```
USE_JSX=true make
# OR
USE_JSX=true rebar get-deps compile
```

## Quick example

```erlang
-module(hello).
-compile({parse_transform, leptus_pt}).

%% leptus callbacks
-export([init/3]).
-export([get/3]).
-export([terminate/3]).

init(_Route, _Req, State) ->
    {ok, State}.

get("/", _Req, State) ->
    {<<"Hello, leptus!">>, State};
get("/hi/:name", Req, State) ->
    Status = 200,
    Name = leptus_req:param(Req, name),
    Body = [{<<"say">>, <<"Hi">>}, {<<"to">>, Name}],
    {Status, {json, Body}, State};
get("/[...]", _Req, State) ->
    Status = not_found,
    Body = [{<<"error">>, <<"not found">>}],
    {Status, {json, Body}, State}.

terminate(_Reason, _Req, _State) ->
    ok.
```

```
$ erl -pa ebin deps/*/ebin
```

```erlang
1> c(hello).
2> leptus:start_listener(http, [{'_', [{hello, undefined_state}]}]).
Leptus 0.3.4 started on http://127.0.0.1:8080
```

```
$ curl localhost:8080/hi/Leptus
{"say":"Hi","to":"Leptus"}

$ curl localhost:8080/some-uri
{"error":"not found"}
```

## Features

* Supports `GET`, `PUT`, `POST` and `DELETE` HTTP methods
* Can respond in plain text, JSON or MessagePack
* Supports basic authentication
* Can be upgraded while it's running (no stopping is required)
* Supports HTTPS and SPDY

## Documentation

Check out the [docs](docs) directory.

## Support

* [Issue tracker](https://github.com/s1n4/leptus/issues)
* #leptus on Freenode

## License

MIT, see LICENSE file for more details.

[![Bitdeli Badge](https://d2weczhvl823v0.cloudfront.net/s1n4/leptus/trend.png)](https://bitdeli.com/free "Bitdeli Badge")
