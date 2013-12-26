-module(leptus_pt).
-author("Sina Samavati <sina.samv@gmail.com>").

-export([parse_transform/2]).


parse_transform(AST, _Options) ->
    put(routes, []),
    walk_ast(AST, []).

%% internal
walk_ast([], Acc) ->
    add_allowed_methods_fun(add_routes_fun(Acc));
walk_ast([{attribute, _, export, _}=H|T], Acc) ->
    case is_transformed(export_funcs) of
        true ->
            walk_ast(T, Acc ++ [H]);
        _ ->
            %% export routes/0 and allowed_methods/1
            walk_ast(T, Acc ++ [export_funcs(H)])
    end;
walk_ast([{function, _, Method, 3, _}=H|T], Acc)
  when Method =:= get; Method =:= put; Method =:= post; Method =:= delete ->
    case is_transformed(Method) of
        true ->
            walk_ast(T, Acc ++ [H]);
        _ ->
            %% collect routes
            walk_ast(T, Acc ++ [check_clauses(H)])
    end;
walk_ast([H|T], Acc) ->
    walk_ast(T, Acc ++ [H]).

%% export routes/0 and allowed_methods/1
export_funcs({attribute, L, export, Funcs}) ->
    transformed(export_funcs),
    {attribute, L, export, Funcs ++ [{routes, 0}, {allowed_methods, 1}]}.

%% check functions' head
check_clauses({function, _, Method, 3, Clause}=H) ->
    %% collect routes
    F = fun({clause, _, E, _, _}=Token) ->
                %% e.g. get("/", _Req, _State)
                {string, _, Route} = hd(E),
                add_route(Route, Method),
                Token
        end,
    lists:foreach(F, Clause),
    transformed(Method),
    H.

%% append a route to the 'routes' key
add_route(Route, Method) ->
    MethodsList = case get(Route) of
                      undefined ->
                          [];
                      Else ->
                          Else
                  end,
    put(Route, MethodsList ++ [http_method(Method)]),
    put(routes, get(routes) ++ [Route]).

%% add routes/0 to the module
%% i.e. routes() -> [Route].
add_routes_fun(AST) ->
    {eof, L} = lists:keyfind(eof, 1, AST),

    %% remove duplicate elements
    Routes = lists:usort(get(routes)),

    put(routes, Routes),
    AST1 = AST -- [{eof, L}],
    AST1 ++ [
             {function, L, routes, 0,
              [
               {clause, L, [], [],
                [erl_parse:abstract(Routes, [{line, L}])]
               }
              ]
             },
             {eof, L}
            ].

%% add allowed_methods/1 to the module
%% e.g allowed_methods("/") -> <<"GET, PUT">>.
add_allowed_methods_fun(AST) ->
    {eof, L} = lists:keyfind(eof, 1, AST),
    Routes = get(routes),
    AST1 = AST -- [{eof, L}],

    AST1 ++ [
             {function, L, allowed_methods, 1,
              [
               {clause, L, [{string, L, R}], [],
                [erl_parse:abstract(join(get(R)), [{line, L}])]
               } || R <- Routes
              ]
             },
             {eof, L}
            ].

%% give X the value 'true'
transformed(X) ->
    put(X, true).

%% check if X has a value
is_transformed(X) ->
    get(X) =/= undefined.

http_method(get) -> <<"GET">>;
http_method(put) -> <<"PUT">>;
http_method(post) -> <<"POST">>;
http_method(delete) -> <<"DELETE">>.

join(Methods) ->
    <<", ", Allow/binary>> = << <<", ", M/binary>> || M <- Methods >>,
    Allow.
