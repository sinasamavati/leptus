%% This file is part of leptus, and released under the MIT license.
%% See LICENSE for more information.

-module(leptus_json).

-export([encode/1]).
-export([decode/1]).
-export([parser/0]).

-ifdef(USE_JSX).
-define(ENCODE(Term), jsx:encode(Term)).
-define(DECODE(Term), jsx:decode(Term)).
-define(PARSER, jsx).
-else.
-define(ENCODE(Term), jiffy:encode({Term})).
-define(DECODE(Term), element(1, jiffy:decode(Term))).
-define(PARSER, jiffy).
-endif.


encode(Term) ->
    ?ENCODE(Term).

decode(Term) ->
    ?DECODE(Term).

parser() ->
    ?PARSER.
