%% This file is part of leptus, and released under the MIT license.
%% See LICENSE for more information.

-module(leptus_router).
-author("Sina Samavati <sina.samv@gmail.com>").

-export([paths/1]).
-export([sort_dispatch/1]).

-include("leptus.hrl").
-type path_rule() :: {[atom() | binary()], term(), module(), any()}.


-spec paths(leptus:handlers()) -> cowboy_router:dispatch_rules().
paths(Handlers) ->
    handle_routes(Handlers, []).

%% internal
handle_routes([], Acc) ->
    Acc;
handle_routes([{HostMatch, X}|T], Acc) ->
    %% each module must have routes/0 -> [string()].
    F = fun({Handler, State}, AccIn) ->
                AccIn ++ [{Route, leptus_handler, new_ctx(Route, Handler, State)}
                          || Route <- Handler:routes()]
        end,
    handle_routes(T, Acc ++ [{HostMatch, lists:foldl(F, [], X)}]).

-spec new_ctx(route(), handler(), handler_state()) -> ctx().
new_ctx(Route, Handler, HandlerState) ->
    #ctx{route=Route, handler=Handler, handler_state=HandlerState}.

%% public
%% order routes the way it matters in cowboy
-spec sort_dispatch(Dispatch) ->
                           Dispatch when Dispatch::cowboy_router:dispatch_rules().
sort_dispatch(Dispatch) ->
    sort_dispatch(Dispatch, []).

%% internal
sort_dispatch([], Acc) ->
    Acc;
sort_dispatch([{HM, C, PathRules}|Rest], Acc) ->
    sort_dispatch(Rest, Acc ++ [{HM, C, sort_path_rules(PathRules)}]).

-spec sort_path_rules([path_rule()]) -> [path_rule()].
sort_path_rules([]) ->
    [];
sort_path_rules([Pivot|Rest]) ->
    Y = segments_length(Pivot),
    sort_path_rules([PathRule || PathRule <- Rest, lt(PathRule, Y)])
        ++ [Pivot] ++
        sort_path_rules([PathRule || PathRule <- Rest, egt(PathRule, Y)]).

-spec segments_length(path_rule()) -> non_neg_integer().
segments_length({Segments, _, _, _}) ->
    F = fun(Segment, {NBSQ, BSQ}) ->
                %% NBSQ :: non-binding segment quantity
                %% BSQ :: binding-segment quantity
                %% if segment is an atom, it's a binding
                case is_atom(Segment) of
                    false -> {NBSQ + 0.5, BSQ};
                    true -> {NBSQ, BSQ + 1}
                end
        end,
    {N1, N2} = lists:foldl(F, {0, 0}, Segments),
    N1 + N2.

%% less than
-spec lt(path_rule(), integer()) -> boolean().
lt(PathRule, Y) ->
    case segments_length(PathRule) of
        N when N < Y -> true;
        _ -> false
    end.

%% equal greater than
-spec egt(path_rule(), integer()) -> boolean().
egt(PathRule, Y) ->
    case segments_length(PathRule) of
        N when N >= Y -> true;
        _ -> false
    end.
