# Guide

First off, let me talk about a *thing* that makes Leptus simple. Parse transformation, the *thing* that saves us some headache, so now we've got a rule,
using the module `leptus_pt` for doing the parse transformation when compiling our request handlers (modules).

So, I suggest you using the following in your request handlers (modules):

```erlang
-compile({parse_transform, leptus_pt}).
```

## Table of contents

* [Callbacks](callbacks.md)
* [Configuration](configuration.md)
