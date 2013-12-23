# Configuration

Leptus configuration file must be named `leptus.config` and put in `priv` directory.

There are two types of configuration that can be defined:

  * [listener (basic listener configuration)](#listener)
  * [handlers (request handlers)](#handlers)

#### listener

This means `http`, `https` and/or `spdy`.

IP address and port number have default values: `127.0.0.1:8080`, also host match is set to `'_'` by default, but you can override them as follows:

```erlang
{Listener :: http | https | spdy,
 [
  {ip, inet:ip_address()},
  {port, inet:port_number()},
  {hostmatch, cowboy_router:dispatch_match()}
 ]
}.
```

It depends on what kind of listener you want to start, for example, if you call [leptus:start_http/2](start.md#start_http2), leptus will ask for
`{http, [...]}` (if it's defined), `{https, [...]}` for https and `{spdy, [...]}` for spdy.

Note that `https` and `spdy` require three options that are `cacertfile`, `certfile` and `keyfile`:
```erlang
{https | spdy,
 [
  {cacertfile, string()},
  {certfile, string()},
  {keyfile, string()}
 ]
}.
```
These three options should be path to the file from the `priv` directory.

#### handlers

Leptus must know your request handlers and their states, they should be defined as `{handlers, [{module(), State :: any()}]}.` in `leptus.config`

## Example

```erlang
%% leptus.config

{http,
 [
  {ip, {0, 0, 0, 0}},
  {port, 4000},
  {hostmatch, "api.example.org"},
 ]
}.

{https,
 [
  {ip, {0, 0, 0, 0}},
  {port, 4443},
  {hostmatch, "api.example.org"},
  {cacertfile, "ssl/ca.crt"},
  {certfile, "ssl/server.crt"},
  {keyfile, "ssl/server.key"}
 ]
}.

{spdy,
 [
  {ip, {0, 0, 0, 0}},
  {port, 4444},
  {hostmatch, "api.example.org"},
  {cacertfile, "ssl/ca.crt"},
  {certfile, "ssl/server.crt"},
  {keyfile, "ssl/server.key"}
 ]
}.

{handlers,
 [
  {rq_handler, undefined_state}
 ]
}.
```
