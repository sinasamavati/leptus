%% Copyright (c) 2013-2015 Sina Samavati <sina.samv@gmail.com>
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.

-module(leptus_json).

%% -----------------------------------------------------------------------------
%% API
%% -----------------------------------------------------------------------------
-export([encode/1]).
-export([decode/1]).
-export([parser/0]).

-ifdef(USE_JSX).
-define(ENCODE(Term), jsx:encode(Term)).
-define(DECODE(Term), jsx:decode(Term)).
-define(PARSER, jsx).
-else.
-define(USE_JIFFY, true).
-define(ENCODE(Term), jiffy_encode(Term)).
-define(DECODE(Term), jiffy_decode(Term)).
-define(PARSER, jiffy).
-endif.

-type json_term() :: [json_term()]
                   | {binary() | atom(), json_term()}
                   | true
                   | false
                   | null
                   | integer()
                   | float()
                   | binary().
-export_type([json_term/0]).

encode(Term) ->
    ?ENCODE(Term).

decode(Term) ->
    ?DECODE(Term).

parser() ->
    ?PARSER.

-ifdef(USE_JIFFY).

%% -----------------------------------------------------------------------------
%% internal
%% -----------------------------------------------------------------------------
jiffy_encode(Term) ->
    jiffy:encode(before_encode(Term)).

jiffy_decode(Bin) ->
    after_decode(jiffy:decode(Bin)).

%% -----------------------------------------------------------------------------
%% before encoding - after decoding (only for jiffy)
%% -----------------------------------------------------------------------------
before_encode([{}]) -> {[]};
before_encode(Term=[{_,_}|_]) -> {Term};
before_encode(Term) -> Term.

after_decode({[]}) -> [{}];
after_decode({Term}) -> Term;
after_decode(Term) -> Term.

-endif.
