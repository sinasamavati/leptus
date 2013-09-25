-module(leptus_hooks).

-export([console_log/4]).


console_log(_, _, _, Req) ->
    %% [%Y-%m-%d %H:%M:%S] METHOD URL
    {{Year, Month, Day}, {Hour, Min, Sec}} = erlang:localtime(),
    {Method, _} = cowboy_req:method(Req),
    Uri = leptus_req:uri(Req),
    io:format("[~w-~w-~w ~w:~w:~w] ~s ~s~n",
              [Year, Month, Day, Hour, Min, Sec, Method, Uri]),
    Req.
