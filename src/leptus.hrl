%% This file is part of leptus, and released under the MIT license.
%% See LICENSE for more information.

-type route() :: '_' | iodata().
-type handler() :: module().
-type handler_state() :: any().

-record(ctx,
        {
          route :: route(),
          handler :: module(),
          handler_state :: any()
        }).
-type ctx() :: #ctx{}.
