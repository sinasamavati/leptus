# Leptus (work in progress) [![Build Status](https://travis-ci.org/s1n4/leptus.png?branch=master)](https://travis-ci.org/s1n4/leptus)

Leptus is a micro web framework that runs on top of cowboy.

Leptus aims at creating RESTful API.

## Requirements

  * Erlang/OTP R15B or newer
  * [cowboy](https://github.com/extend/cowboy)
  * [jsx](https://github.com/talentdeficit/jsx)

## Installation

Run `make` or add it to your rebar configuration

```
{deps, [
        ...
        {leptus, ".*", {git, "git://github.com/s1n4/leptus.git", {branch, "master"}}}
       ]}.
```

## Quickstart

```erlang
-module(whatever).

%% leptus callbacks
-export([routes/0]).
-export([get/2]).

routes() ->
    ["/", "/hello/:name"].

get("/", _Req) ->
    Status = 200,
    Body = <<"Hello, leptus!">>,
    {Status, Body};

get("/hello/:name", Req) ->
    Status = 200,
    Name = leptus_req:binding(name, Req),
    Body = << <<"Hello, ">>/binary, Name/binary >>,
    {Status, Body}.
```

```
$ erl -pa ebin deps/*/ebin
```

```erlang
1> leptus:start_http({modules, [whatever]}).
```

## Configuration

Leptus configuration file must be named `leptus.config` and put in `priv` directory.

There are two types of configuration that can be defined:

  * http
  * modules (request handlers)

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

#### modules

Leptus must know your request handlers, and they must be defined as `{modules, [module()]}.` in `leptus.config`

#### leptus.config example

Let's assume our current working directory looks like the following:

```
.
├── ebin
│   ├── ...
│   └── rq_handler.beam
│
├── priv
│   ├── ...
│   └── leptus.config
│
├── src
│   ├── ...
│   └── rq_handler.erl
│   ...
```

```erlang
%% leptus.config

{http,
 [
  {ip, "0.0.0.0"},
  {port, 8080}
 ]
}.

{modules,
 [rq_handler]
}.
```

## License

MIT, see LICENSE file for more details.
