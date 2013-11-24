# Types

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

Handlers :: [{module(), State}]
```
