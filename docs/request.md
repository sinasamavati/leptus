# Request

Please keep it in mind that `Req` is exactly cowboy's `Req` object, but in Leptus, it must not be modified.

All the functions given below are accessible through the `leptus_req` module,
which are supposed to work with the `Req` object.

* [param/2](#param2)
* [params/1](#params1)
* [qs/1](#qs1)
* [qs_val/2](#qs_val2)
* [uri/1](#uri1)
* [version/1](#version1)
* [method/1](#method1)
* [body/1](#body1)
* [body_raw/1](#body_raw1)
* [body_qs/1](#body_qs1)
* [header/2](#header2)
* [parse_header/2](#parse_header2)
* [auth/2](#auth2)

#### param/2

Returns a parameter that is bound to the route.

```erlang
param(Name :: atom(), Req) -> binary() | undefined

%% e.g.
%% route: /items/:id
%% requested uri: /items/1863
param(id, Req) -> <<"1863">>
```

#### params/1

Returns parameters that are bound to the route.

```erlang
params(Req) -> [{atom(), binary()}]

%% e.g.
params(Req) -> [{id, <<"1863">>}]
```

#### qs/1

Returns query strings.

```erlang
qs(Req) -> binary()

%% e.g.
%% uri: /items/?limit=50
qs(Req) -> <<"limit=50">>
```

#### qs_val/2

Returns the given query string value.

```erlang
qs_val(Field :: binary(), Req()) -> binary() | undefined

%% e.g.
qs_val(<<"limit">>, Req) -> <<"50">>
```

#### uri/1

Returns the requested URI.

```erlang
uri(Req) -> binary()

%% e.g.
uri(Req) -> <<"/items/?limit=50">>
```

#### version/1

Returns HTTP version.

```erlang
version(Req) -> 'HTTP/1.1' | 'HTTP/1.0'

%% e.g.
version(Req) -> 'HTTP/1.1'
```

#### method/1

Returns used HTTP method.

```erlang
method(Req) -> binary()

%% e.g.
method(Req) -> <<"DELETE">>
```

#### body/1

Returns received body (decoding might apply to it).

```erlang
body(Req) -> binary() | json_term()

%% e.g.
body(Req) -> <<"foo=bar">>

%% when content-type is set to applicaation/json
body(Req) -> [{<<"function">>, <<"body/1">>}]
```

#### body_raw/1

Returns raw body.

```erlang
body_raw(Req) -> binary()

%% e.g.
body_raw(Req) -> <<"{\"function\": \"body/1\"}">>
```

#### body_qs/1

Returns body but in query string format.

```erlang
body_qs(Req) -> [{binary(), binary() | true}]

%% e.g.
body_qs(Req) -> [{<<"foo">>, <<"bar">>}]
```

#### header/2

Returns the given header value.

```erlang
header(binary(), Req) -> binary()

%% e.g.
header(<<"content-type">>, Req) -> <<"application/x-www-form-urlencoded">>
```

#### parse_header/2

Parses the given header.

```erlang
parse_header(binary(), Req) -> any() | <<>>

%% e.g.
parse_header(<<"content-type">>, Req) -> {<<"application">>, <<"json">>, []}
```

#### auth/2

Checks for the given authorization method.

NOTE: basic authentication is only supported at the moment.

```erlang
auth(<<"basic">>, Req) -> {binary(), binary()} | <<>> | error

%% e.g.
auth(<<"basic">>, Req) -> {<<"username">>, <<"p4ssw0rd">>}
```
