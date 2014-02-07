%% This file is part of leptus, and released under the MIT license.
%% See LICENSE for more information.

-module(leptus_router).
-author("Sina Samavati <sina.samv@gmail.com>").

-export([paths/1]).
-export([sort_dispatch/1]).

-type handler() :: module().
-type route() :: cowboy_router:route_match().
-type handler_state() :: any().

-record(ctx,
        {
          handler :: handler(),
          route :: route(),
          handler_state :: handler_state()
        }).
-type ctx() :: #ctx{}.


-spec paths([{handler(), handler_state()}]) -> cowboy_router:dispatch_rules().
paths(Handlers) ->
    handle_routes(fetch_routes(Handlers, []), []).

%% internal
-spec fetch_routes([{handler(), handler_state()}], []) -> [ctx()].
fetch_routes([], Acc) ->
    Acc;
fetch_routes([{Handler, State}|T], Acc) ->
    %% each module must have routes/0 -> [string()].
    Ctx = [#ctx{handler=Handler, route=Route, handler_state=State}
           ||  Route <- Handler:routes()],
    fetch_routes(T, Acc ++ Ctx).

-spec handle_routes([ctx()], [none() | {route(), leptus_handler, ctx()}])
                   -> [cowboy_router:route_path()].
handle_routes([], Acc) ->
    Acc;
handle_routes([Ctx|T], Acc) ->
    handle_routes(T, Acc ++ [{Ctx#ctx.route, leptus_handler, Ctx}]).

%% public
%% order routes the way it matters in cowboy
-spec sort_dispatch(cowboy_router:dispatch_rules()) ->
                           cowboy_router:dispatch_rules().
sort_dispatch(Dispatch) ->
    sort_dispatch(Dispatch, []).

%% internal
-spec sort_dispatch(cowboy_router:dispatch_rules(), []) ->
                           cowboy_router:dispatch_rules().
sort_dispatch([], Acc) ->
    Acc;
sort_dispatch([{HM, C, PathRules}|Rest], Acc) ->
    sort_dispatch(Rest, Acc ++ [{HM, C, sort_path_rules(PathRules)}]).

sort_path_rules(PathRules) ->
    sort_path_rules(PathRules, [], [], []).

sort_path_rules([], High, Medium, Low) ->
    High ++ Medium ++ Low;
sort_path_rules([{Segments, _, _, _}=PathRule|Rest], High, Medium, Low) ->
    F = fun(Segment, {NBSQ, BSQ}) ->
                %% NBSQ :: non-binding segment quantity
                %% BSQ :: binding-segment quantity
                %% if segment is an atom, it's a binding
                case is_atom(Segment) of
                    false -> {NBSQ + 1, BSQ};
                    true -> {NBSQ, BSQ + 1}
                end
        end,
    {High1, Medium1, Low1} = case lists:foldl(F, {0, 0}, Segments) of
                                 {0, 0} ->
                                     {[PathRule|High], Medium, Low};
                                 {0, 1} ->
                                     {High, Medium ++ [PathRule], Low};
                                 {N1, N2} ->
                                     if N1 > N2 ->
                                             {High ++ [PathRule], Medium, Low};
                                        N1 =:= N2 ->
                                             {High, Medium ++ [PathRule], Low};
                                        N1 < N2 ->
                                             {High, Medium, Low ++ [PathRule]}
                                     end
                             end,
    sort_path_rules(Rest, High1, Medium1, Low1).
