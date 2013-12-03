# Types

```erlang
json_term() = [json_term()]
              | {binary() | atom(), json_term()}
              | true
              | false
              | null
              | integer()
              | float()
              | binary()

Route   :: cowboy_router:route_match()
Req     :: cowboy_req:req()
Status  :: non_neg_integer() | binary()
Body    :: string() | binary() | {json, json_term()}
Headers :: [{binary(), iodata()}]
State   :: any()

Handlers :: [{module(), State}]
```
