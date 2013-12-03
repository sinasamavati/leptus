# Start

#### start_http/0

Starts leptus and it's dependencies, also note that priv/leptus.config must be existing

```erlang
leptus:start_http() -> {ok, pid()}
```

#### start_http/1

Such as [start_http/0](#start_http0), but requires the `Handlers` argument.

```erlang
leptus:start_http(Handlers) -> {ok, pid()}
```

## The OTP way

The way everyone recommends for starting an OTP application:
```erlang
application:start(crypto),
application:start(ranch),
application:start(cowboy),
application:start(leptus).
```
and then [start_http/0](#start_http0) or [start_http/1](#start_http1) should be called.
