%% This file is part of leptus, and released under the MIT license.
%% See LICENSE for more information.

-module(leptus_routes2).

-export([routes/0]).


routes() ->
    ["/something/:key", "/something/else"].
