-module(leptus_pt).
-author("Sina Samavati <sina.samv@gmail.com>").

-export([parse_transform/2]).


parse_transform(AST, _Options) ->
    put(routes, []),
    do_transform(AST, []).

%% internal
do_transform([], Acc) ->
    add_aw_fun(add_routes_fun(Acc));
do_transform([{attribute, _, export, _}=H|T], Acc) ->
    case is_transformed(export_funcs) of
        true ->
            do_transform(T, Acc ++ [H]);
        _ ->
            %% export routes/0 if it's not done
            do_transform(T, Acc ++ [export_funcs(H)])
    end;
do_transform([{function, _, Method, 3, _}=H|T], Acc)
  when Method =:= get; Method =:= put; Method =:= post; Method =:= delete ->
    case is_transformed(Method) of
        true ->
            do_transform(T, Acc ++ [H]);
        _ ->
            %% collect routes
            do_transform(T, Acc ++ [transform_clause(H)])
    end;
do_transform([H|T], Acc) ->
    do_transform(T, Acc ++ [H]).

%% export routes/0 and allowed_methods/1.
export_funcs({attribute, L, export, Funcs}) ->
    transformed(export_funcs),
    {attribute, L, export, Funcs ++ [{routes, 0}, {allowed_methods, 1}]}.

%% check functions' head
transform_clause({function, _, Method, 3, Clause}=H) ->
    %% collect routes
    F = fun({clause, _, E, _, _}=Token) ->
                %% e.g. get("/", _Req)
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
             {function, L + 1, routes, 0,
              [
               {clause, L + 1, [], [],
                [erl_parse:abstract(Routes, [{line, L + 2}])]
               }
              ]
             },
             {eof, L + 3}
            ].

%% add allowed_methods/1
%% e.g allowed_methods("/") -> [<<"GET">>, <<"PUT">>].
add_aw_fun(AST) ->
    {eof, L} = lists:keyfind(eof, 1, AST),
    Routes = get(routes),
    put(line, L),
    AST1 = AST -- [{eof, L}],

    AST1 ++ [
             {function, L + 1, allowed_methods, 1,
              [
               {clause, get(line) + 1, [{string, get(line) + 1, R}], [],
                [erl_parse:abstract(get(R), [{line, get(line) + 2}])]
               } || R <- Routes, _ <- [put(line, L + 1)]
              ]
             },
             {eof, get(line) + 1}
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
