# Start

## Types

```erlang
handler() = {module(), State :: any()}
handlers() = [handler()]

listener() = http | https | spdy
listener_option() = {ip, inet:ip_address()}
                  | {port, inet:port_number()}
                  | {hostmatch, cowboy_router:dispatch_match()}
                  | {cacertfile, file:name_all()}
                  | {certfile, file:name_all()}
                  | {keyfile, file:name_all()}

option() = {handlers, handlers()} | {listener(), [listener_option()]}
options() = [option()]

app_name() = atom()
```

#### start_http/1

Starts leptus' dependencies and an HTTP listener.

```erlang
leptus:start_http(OptionsOrAppName) -> {ok, pid()}
OptionsOrAppName = options() | app_name()
```

Note that if you use `app_name()`, Leptus will try to read `app_name/priv/leptus.config` file
which should contain `options()`.

#### start_https/1

Starts leptus' dependencies and an HTTPS listener.

```erlang
leptus:start_https(OptionsOrAppName) -> {ok, pid()}
OptionsOrAppName = options() | app_name()
```

This requires specifying `cacertfile`, `certfile` and `keyfile`.

#### start_spdy/1

Starts leptus' dependencies and an SPDY listener.

```erlang
leptus:start_spdy(Handlers, Options) -> {ok, pid()}
OptionsOrAppName = options() | app_name()
```

Requires the same things as [start_https/1](#start_https1).

## The OTP way

The way everyone recommends for starting an OTP application:
```erlang
application:start(crypto),
application:start(ranch),
application:start(cowboy),
application:start(leptus).
```
and then one of the functions that are described above should be called to start a listener.
