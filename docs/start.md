# Start

## For test purposes

There are two functions to start leptus for test purposes:
  * `leptus:start_http/0`, considers you've got the leptus.config file in your priv directory.
  * `leptus:start_http/1`, needs the `Handlers` parameter.

## The OTP way

The way everyone recommends for starting an OTP application:
```erlang
application:start(crypto),
application:start(ranch),
application:start(cowboy),
application:start(leptus). %% priv/leptus.config must be existing
```
