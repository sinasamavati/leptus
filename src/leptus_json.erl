-module(leptus_json).

-export([encode/1]).
-export([decode/1]).

-ifdef(USE_JSX).
-define(ENCODE(Term), jsx:encode(Term)).
-define(DECODE(Term), jsx:decode(Term)).
-else.
-define(ENCODE(Term), jiffy:encode({Term})).
-define(DECODE(Term), element(1, jiffy:decode(Term))).
-endif.


encode(Term) ->
    ?ENCODE(Term).

decode(Term) ->
    ?DECODE(Term).
