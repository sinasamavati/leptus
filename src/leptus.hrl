%% This file is part of leptus, and released under the MIT license.
%% See LICENSE for more information.

-record(ctx,
        {
          route :: cowboy_router:route_match(),
          handler:: module(),
          handler_state :: any()
        }).
-type ctx() :: #ctx{}.
