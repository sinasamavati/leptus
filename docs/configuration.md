# Configuration

Leptus configuration file must be named `leptus.config` and put in `priv` directory.

There are two types of configuration that can be defined:

  * [http (basic http configuration)](#http)
  * [handlers (request handlers)](#handlers)

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

## Example

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
