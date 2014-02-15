%% This file is part of leptus, and released under the MIT license.
%% See LICENSE for more information.

-module(leptus_router).
-author("Sina Samavati <sina.samv@gmail.com>").

-export([paths/1]).
-export([sort_dispatch/1]).

-include("leptus.hrl").

-type route() :: cowboy_router:route_match().
-type handler() :: module().
-type handler_state() :: any().

-type path() :: {term(), [{route(), handler(), handler_state()}]}.
-type path_rule() :: {[atom() | binary()], term(), module(), any()}.


-spec paths(leptus:handlers()) -> cowboy_router:dispatch_rules().
paths(Handlers) ->
    handle_routes(Handlers, []).

%% internal
-spec handle_routes(leptus:handlers(), []) -> [path()].
handle_routes([], Acc) ->
    Acc;
handle_routes([{HostMatch, X}|T], Acc) ->
    %% each module must have routes/0 -> [string()].
    F = fun({Handler, State}, AccIn) ->
                AccIn ++ [{Route, leptus_handler, ctx(Route, Handler, State)}
                          || Route <- Handler:routes()]
        end,
    handle_routes(T, Acc ++ [{HostMatch, lists:foldl(F, [], X)}]).

ctx(Route, Handler, HandlerState) ->
    #ctx{route=Route, handler=Handler, handler_state=HandlerState}.

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


%% tests
-ifdef(TEST).

paths_test() ->
    Ctx1 = #ctx{handler=leptus_routes1, route="/", handler_state=[]},
    Ctx2 = #ctx{handler=leptus_routes1, route="/blah", handler_state=[]},
    Ctx3 = #ctx{handler=leptus_routes1, route="/hello/:name", handler_state=[]},
    Ctx4 = #ctx{handler=leptus_routes1, route="/some-url/to/some-path",
                handler_state=[]},
    Ctx5 = #ctx{handler=leptus_routes2, route="/something/:key",
                handler_state=aha},
    Ctx6 = #ctx{handler=leptus_routes2, route="/something/else",
                handler_state=aha},
    Ctx7 = #ctx{handler=leptus_routes3, route="/users/:id",
                handler_state=i_see},
    Ctx8 = #ctx{handler=leptus_routes3, route="/users/:id/info",
                handler_state=i_see},
    [{'_', [
            {"/", leptus_handler, Ctx1},
            {"/blah", leptus_handler, Ctx2},
            {"/hello/:name", leptus_handler, Ctx3},
            {"/some-url/to/some-path", leptus_handler, Ctx4},
            {"/something/:key", leptus_handler, Ctx5},
            {"/something/else", leptus_handler, Ctx6},
            {"/users/:id", leptus_handler, Ctx7},
            {"/users/:id/info", leptus_handler, Ctx8}
           ]
     }]= leptus_router:paths([{'_', [{leptus_routes1, []},
                                     {leptus_routes2, aha},
                                     {leptus_routes3, i_see}]}]).

sort_dispatch_test() ->
    Routes = [
              {"/:bucket", handler, undefined},
              {"/_version", handler, undefined},
              {"/:bucket/:key", handler, undefined},
              {"/:bucket/_keys", handler, undefined},
              {"/_buckets", handler, undefined},
              {"/", handler, undefined}
             ],
    Dispatch = cowboy_router:compile([{'_', Routes}]),
    [
     {'_',[],
      [{[],[],handler,undefined},
       {[<<"_version">>],[],handler,undefined},
       {[<<"_buckets">>],[],handler,undefined},
       {[bucket],[],handler,undefined},
       {[bucket,<<"_keys">>],[],handler,undefined},
       {[bucket,key],[],handler,undefined}]}
    ] = leptus_router:sort_dispatch(Dispatch),

    Routes1 = [{"/:a/:b", handler, undefined},
               {"/:a/:b/x/:d", handler, undefined},
               {"/:a", handler, undefined},
               {"/:a/:b/:c", handler, undefined},
               {"/", handler, undefined},
               {"/:a/:b/x", handler, undefined},
               {"/:a/x/:c/:d", handler, undefined},
               {"/:a/x", handler, undefined}],
    Dispatch1 = cowboy_router:compile([{'_', Routes1}]),
    [{'_', [],
      [
       {[], [], handler, undefined},
       {[a], [], handler, undefined},
       {[a, <<"x">>], [], handler, undefined},
       {[a, b],[], handler, undefined},
       {[a, b, <<"x">>], [], handler, undefined},
       {[a, b, c], [], handler, undefined},
       {[a, b, <<"x">>, d], [], handler, undefined},
       {[a, <<"x">>, c, d], [], handler, undefined}
      ]}] = leptus_router:sort_dispatch(Dispatch1).

-endif.
