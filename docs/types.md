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

Route    :: cowboy_router:route_match()
Req      :: cowboy_req:req()
Status   :: non_neg_integer() | binary()
Body     :: string() | binary() | {json | msgpack, json_term()}
Headers  :: [{binary(), iodata()}]
Options  :: [{priv_dir App :: atom()}]
Handlers :: [{module(), State :: any()}]
```

At the moment, `Options` is just to declare which `priv` directory should be used to read `leptus.config` file from.
