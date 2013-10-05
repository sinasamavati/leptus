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
