%% This file is part of leptus, and released under the MIT license.
%% See LICENSE for more information.

-module(leptus_routes1).

-export([routes/0]).


routes() ->
    ["/", "/blah", "/hello/:name", "/some-url/to/some-path"].
