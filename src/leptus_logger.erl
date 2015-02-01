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

-module(leptus_logger).

-include("leptus_logger.hrl").

%% -----------------------------------------------------------------------------
%% API
%% -----------------------------------------------------------------------------
-export([add_handler/2]).
-export([delete_handler/2]).
-export([send_event/2]).
-export([format/2]).

-spec add_handler(atom() | {atom(), any()}, any()) ->
                         ok | {'EXIT', any()} | any().
add_handler(Mod, Args) ->
    gen_event:add_handler(?LOGGER, Mod, Args).

-spec delete_handler(atom() | {atom(), any()}, any()) ->
                            any() | {error, module_not_found} | {'EXIT', any()}.
delete_handler(Mod, Args) ->
    gen_event:delete_handler(?LOGGER, Mod, Args).

-spec send_event(atom(), log_data()) -> ok.
send_event(Event, LogData) ->
    gen_event:sync_notify(?LOGGER, {Event, LogData}).

-spec format(string(), log_data()) -> string().
format(Fmt, LogData) ->
    %% e.g.
    %% "~h ~l ~u ~t \"~r\" ~s ~B", LogData ->
    %%   127.0.0.1 - - [11/Jun/2014:03:07:25 +0450] "GET /b?p=2 HTTP/1.1" 200 83
    format(Fmt, LogData, []).

%% -----------------------------------------------------------------------------
%% internal
%% -----------------------------------------------------------------------------
-spec format(string(), log_data(), iolist()) -> string().
format([], _, Acc) ->
    lists:flatten(lists:reverse(Acc));
format([$~, $h|Fmt], LD=#log_data{ip=IP}, Acc) ->
    format(Fmt, LD, [inet_parse:ntoa(IP)|Acc]);
format([$~, $l|Fmt], LD, Acc) ->
    format(Fmt, LD, [$-|Acc]);
format([$~, $u|Fmt], LD, Acc) ->
    format(Fmt, LD, [$-|Acc]);
format([$~, $t|Fmt], LD=#log_data{request_time=Datetime}, Acc) ->
    format(Fmt, LD, [[$[, datetime(Datetime), $]]|Acc]);
format([$~, $r|Fmt], LD=#log_data{method=M, uri=U, version=V}, Acc) ->
    format(Fmt, LD, [[binary_to_list(M), $ ,
                      binary_to_list(U), $ ,
                      atom_to_list(V)]|Acc]);
format([$~, $s|Fmt], LD=#log_data{status=S}, Acc) ->
    format(Fmt, LD, [integer_to_list(S)|Acc]);
format([$~, $b|Fmt], LD=#log_data{content_length=0}, Acc) ->
    format(Fmt, LD, [$-|Acc]);
format([$~, $b|Fmt], LD=#log_data{content_length=B}, Acc) ->
    format(Fmt, LD, [integer_to_list(B)|Acc]);
format([$~, $B|Fmt], LD=#log_data{content_length=B}, Acc) ->
    format(Fmt, LD, [integer_to_list(B)|Acc]);
format([$~, ${|Fmt], LD=#log_data{headers=Headers}, Acc) ->
    {Name, Fmt1} = get_name(Fmt, []),
    format(Fmt1, LD, [get_value(Name, Headers)|Acc]);
format([H|Fmt], LD, Acc) ->
    format(Fmt, LD, [H|Acc]).

-spec get_name(string(), string()) -> {binary(), string()}.
get_name([$}|Fmt], Acc) ->
    {list_to_binary(lists:reverse(Acc)), Fmt};
get_name([H|Fmt], Acc) ->
    get_name(Fmt, [H|Acc]).

-spec get_value(binary(), [{binary(), iodata()}]) -> list().
get_value(K, Props) ->
    case lists:keyfind(K, 1, Props) of
        {K, V} -> binary_to_list(V);
        _ -> "-"
    end.

-spec month(1..12) -> string().
month(1) -> "Jan";
month(2) -> "Feb";
month(3) -> "Mar";
month(4) -> "Apr";
month(5) -> "May";
month(6) -> "Jun";
month(7) -> "Jul";
month(8) -> "Aug";
month(9) -> "Sep";
month(10) -> "Oct";
month(11) -> "Nov";
month(12) -> "Dec".

-spec timezone() -> io_lib:chars().
timezone() ->
    {DiffH, DiffM} = timezone(erlang:universaltime(), erlang:localtime()),
    %% Ugly reformatting code to get times like +0000 and -1300
    if DiffH < 0 ->
            io_lib:format("-~2..0w~2..0w", [abs(DiffH), abs(DiffM)]);
       true ->
            io_lib:format("+~2..0w~2..0w", [DiffH, DiffM])
    end.

-spec timezone(calendar:datetime(), calendar:datetime()) ->
                      {integer(), integer()}.
timezone(UniversalTime, LocalTime) ->
    DiffSecs = calendar:datetime_to_gregorian_seconds(LocalTime) -
        calendar:datetime_to_gregorian_seconds(UniversalTime),
    Mins = (DiffSecs / 60),
    H = trunc(Mins / 60),
    M =  trunc(Mins - (H * 60)),
    {H, M}.

-spec datetime(calendar:datetime()) -> string().
datetime({{Y, M, D}, {H, Mi, S}}) ->
    io_lib:format("~w/~s/~w:~2..0w:~2..0w:~2..0w ~s",
                  [D, month(M), Y, H, Mi, S, timezone()]).
