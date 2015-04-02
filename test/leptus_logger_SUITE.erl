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

-module(leptus_logger_SUITE).

-export([all/0]).
-export([datetime/1]).
-export([format/1]).

-include("leptus_logger.hrl").

all() ->
    [datetime, format].

datetime(_) ->
    Datetime = erlang:localtime(),
    true = is_list(leptus_logger:datetime(Datetime)),
    ok.

format(_) ->
    Fmt1 = "\"~r\" ~s ~B \"~{asdf}\" ok \"~{user-agent}\"!",
    LD1 = #log_data{headers = [{<<"user-agent">>, <<"curl">>}]},
    "\"GET / HTTP/1.1\" 200 0 \"-\" ok \"curl\"!" =
        leptus_logger:format(Fmt1, LD1),

    Fmt2 = "~h ~l ~u ~l \"~r\" ~s ~b",
    LD2 = #log_data{ip = {127, 0, 0, 1}, uri = <<"/resource/a">>,
                    method = <<"DELETE">>, status = 204},
    "127.0.0.1 - - - \"DELETE /resource/a HTTP/1.1\" 204 -" =
        leptus_logger:format(Fmt2, LD2),
    ok.
