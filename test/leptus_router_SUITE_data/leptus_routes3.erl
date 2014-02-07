%% This file is part of leptus, and released under the MIT license.
%% See LICENSE for more information.

-module(leptus_routes3).

-export([routes/0]).


routes() ->
    ["/users/:id", "/users/:id/info"].
