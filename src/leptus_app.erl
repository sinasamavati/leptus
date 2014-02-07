%% This file is part of leptus, and released under the MIT license.
%% See LICENSE for more information.

-module(leptus_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    leptus_sup:start_link().

stop(_State) ->
    ok.
