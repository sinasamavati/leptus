# Documentations

## Preface

Let me talk about a *thing* that makes Leptus simple. Parse transform, the *thing* that saves us some headache, so now we've got a rule,
using the module `leptus_pt` for doing the parse transform when compiling our request/resource handlers (modules).

So, I suggest you using the following in your request/resource handlers (modules):

```erlang
-compile({parse_transform, leptus_pt}).
```

## Table of contents

* [Types](types.md)

* [Callbacks](callbacks.md)
  * [prefix/0](callbacks.md#prefix0)
  * [init/3](callbacks.md#init3)
  * [cross_domains/3](callbacks.md#cross_domains3)
  * [is_authorized/3](callbacks.md#is_authorized3)
  * [HttpMethod/3](callbacks.md#httpmethod3)
  * [terminate/4](callbacks.md#terminate4)

* [Request](request.md)
  * [param/2](request.md#param2)
  * [params/1](request.md#params1)
  * [qs/1](request.md#qs1)
  * [qs_val/2](request.md#qs_val2)
  * [uri/1](request.md#uri1)
  * [version/1](request.md#version1)
  * [method/1](request.md#method1)
  * [body/1](request.md#body1)
  * [body_raw/1](request.md#body_raw1)
  * [body_qs/1](request.md#body_qs1)
  * [header/2](request.md#header2)
  * [parse_header/2](request.md#parse_header2)
  * [auth/2](request.md#auth2)

* [Configuration](configuration.md)
  * [listener (basic listener configuration)](configuration.md#listener)
  * [handlers (request handlers)](configuration.md#handlers)

* [Start](start.md)
  * [start_http/1](start.md#start_http1)
  * [start_https/1](start.md#start_https1)
  * [start_spdy/1](start.md#start_spdy1)
  * [The OTP way](start.md#the-otp-way)
