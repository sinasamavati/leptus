# Start

#### start_http/1

Starts leptus' dependencies and an HTTP listener.

```erlang
Handlers :: [{module(), State :: any()}]
leptus:start_http(Handlers) -> {ok, pid()}
```

#### start_http/2

Such as [start_http/1](#start_http1), but requires the `Options` argument too.

```erlang
Options :: [{priv_dir, App :: atom()}]
leptus:start_http(Handlers, Options) -> {ok, pid()}
```

Note that if you declared `Handlers` in your `leptus.config` file, you should leave the first argument as an empty list,
also it should be declared that which `priv` directory `leptus.config` should be read from.

#### start_https/2

Starts leptus' dependencies and an HTTPS listener.

```erlang
leptus:start_https(Handlers, Options) -> {ok, pid()}
```

Works like [start_http/2](#start_http2), requires specifying `cacertfile`, `certfile`
and `keyfile` as well. Check out the [listener](configuration.md#listener) part of configuration for more information about these three options.

#### start_spdy/2

Starts leptus' dependencies and an SPDY listener.

```erlang
leptus:start_spdy(Handlers, Options) -> {ok, pid()}
```

Requires the same things as [start_https/2](#start_https2).

## The OTP way

The way everyone recommends for starting an OTP application:
```erlang
application:start(crypto),
application:start(ranch),
application:start(cowboy),
application:start(leptus).
```
and then one of the functions that are described above should be called to start a listener.
