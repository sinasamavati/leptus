# Guide

First off, let me talk about a *thing* that makes Leptus simple. Parse transformation, the *thing* that saves us some headache, so now we've got a rule,
using the module `leptus_pt` for doing the parse transformation when compiling our request handlers (modules).

So, I suggest you using the following in your request handlers (modules):

```erlang
-compile({parse_transform, leptus_pt}).
```

## Types

```erlang
json() = [json()]
    | [{binary() | atom(), json_term()}]
    | true
    | false
    | null
    | integer()
    | float()
    | binary()

Status  :: non_neg_integer() | binary()
Body    :: string() | binary() | json()
Headers :: [{binary(), iodata()}] | json
State   :: any()
```

## Table of contents

* [Callbacks](callbacks.md)
  * [init/3](callbacks.md#init3)
  * [is_authorized/3](callbacks.md#isauthorized_3)
  * [HttpMethod/3](callbacks.md#httpmethod3)
  * [terminate/3](callbacks.md#terminate3)

* [Configuration](configuration.md)
  * [http (basic http configuration)](configuration.md#http)
  * [handlers (request handlers)](configuration.md#handlers)
