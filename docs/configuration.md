# Configuration

Leptus configuration file must be named `leptus.config` and put in `priv` directory.

There are two types of configuration that can be defined:

  * [http (basic http configuration)](#http)
  * [handlers (request handlers)](#handlers)

#### http

IP address and port number have default values: `127.0.0.1:8080`, but you can override them as follows:

```erlang
{http,
 [
  {ip, inet:ip_address()},
  {port, inet:port_number()}
 ]
}.
```

#### handlers

Leptus must know your request handlers and their states, they should be defined as `{handlers, [{module(), State::any()}]}.` in `leptus.config`

## Example

```erlang
%% leptus.config

{http,
 [
  {ip, {0, 0, 0, 0}},
  {port, 4000}
 ]
}.

{handlers,
 [
  {rq_handler, undefined_state}
 ]
}.
```
