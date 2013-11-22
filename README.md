# Leptus [![Build Status](https://travis-ci.org/s1n4/leptus.png?branch=master)](https://travis-ci.org/s1n4/leptus)

Leptus is an Erlang REST framework that runs on top of cowboy.

Leptus aims at simply creating RESTful APIs.

## Requirements

  * Erlang/OTP R15B or newer
  * [cowboy](https://github.com/extend/cowboy)
  * [jiffy](https://github.com/davisp/jiffy)

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

## Quickstart

```erlang
-module(rq_handler).
-compile({parse_transform, leptus_pt}).

%% leptus callbacks
-export([init/3]).
-export([get/3]).
-export([terminate/3]).

init(_Route, _Req, State) ->
    {ok, State}.

get("/", _Req, State) ->
    {<<"Hello, leptus!">>, State};
get("/hello/:name", Req, State) ->
    Status = 200,
    Name = leptus_req:param(name, Req),
    Body = <<"Hello, ", Name/binary>>,
    {Status, Body, State}.

terminate(_Reason, _Req, _State) ->
    ok.
```

```
$ erl -pa ebin deps/*/ebin
```

```erlang
1> c(rq_handler).
2> Handlers = [{rq_handler, []}].
3> leptus:start_http(Handlers).
```

## Configuration

Leptus configuration file must be named `leptus.config` and put in `priv` directory.

There are two types of configuration that can be defined:

  * http (basic http configuration)
  * handlers (request handlers)

#### http

IP address and port number have default values: `127.0.0.1:8000`, but you can override them as follows:

```erlang
{http,
 [
  {ip, string()},
  {port, integer()}
 ]
}.
```

#### handlers

Leptus must know your request handlers and their states, they should be defined as `{handlers, [{module(), state()}]}.` in `leptus.config`

#### leptus.config example

```erlang
%% leptus.config

{http,
 [
  {ip, "0.0.0.0"},
  {port, 8080}
 ]
}.

{handlers,
 [
  {rq_handler, []}
 ]
}.
```

## License

MIT, see LICENSE file for more details.

## TODO

* Add hooks
* Add examples
* ...
