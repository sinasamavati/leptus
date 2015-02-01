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

-module(leptus_pt).

-export([parse_transform/2]).

parse_transform(AST, _Options) ->
    %% AST, Acc, Routes :: [{Route, AllowedMethod :: binary()}]
    walk_ast(AST, [], []).

%% -----------------------------------------------------------------------------
%% internal
%% -----------------------------------------------------------------------------
walk_ast([], Acc, _) ->
    Acc;
%% -----------------------------------------------------------------------------
%% export routes/0 and allowed_methods/1
%% -----------------------------------------------------------------------------
walk_ast([{attribute, L, module, _}=Form|Rest], Acc, Routes) ->
    ExportForm = {attribute, L, export, [{routes, 0}, {allowed_methods, 1}]},
    walk_ast(Rest, Acc ++ [Form] ++ [ExportForm], Routes);
%% -----------------------------------------------------------------------------
%% collect routes
%% -----------------------------------------------------------------------------
walk_ast([{function, _, Method, 3, _}=Form|Rest], Acc, Routes)
  when Method =:= get; Method =:= put; Method =:= post; Method =:= delete ->
    Routes1 = check_clauses(Form),
    walk_ast(Rest, Acc ++ [Form], Routes ++ Routes1);
%% -----------------------------------------------------------------------------
%% add routes/0 to the module
%% i.e. routes() -> [Route]
%% -----------------------------------------------------------------------------
%% add allowed_methods/1 to the module
%% e.g. allowed_methods("/") -> [<<"GET">>, <<"PUT">>]
%% -----------------------------------------------------------------------------
walk_ast([{eof, L}=Form|Rest], Acc, Routes) ->
    F = fun({Route, Method}, AccIn) ->
                case lists:keyfind(Route, 1, AccIn) of
                    {Route, Methods} ->
                        lists:keystore(Route, 1, AccIn,
                                       {Route, [Method|Methods]});
                    _ ->
                        [{Route, [Method]}|AccIn]
                end
        end,
    %% [{string(), [binary()]}]
    RoutesNMethods = lists:usort(lists:foldr(F, [], Routes)),

    %% [string()]
    Routes1 = [Route || {Route, _} <- RoutesNMethods],

    %% routes() -> [Route]
    Routes0Fun = {function, L, routes, 0,
                  [{clause, L, [], [],
                    [erl_parse:abstract(Routes1, [{line, L}])]}]},

    %% allowed_methods(Route) -> [binary()]
    AllowedMethods1Fun = {function, L, allowed_methods, 1,
                          [{clause, L, [{string, L, Route}], [],
                            [erl_parse:abstract(Methods, [{line, L}])]}
                           || {Route, Methods} <- RoutesNMethods]},
    walk_ast(Rest, Acc ++ [Routes0Fun|[AllowedMethods1Fun]] ++ [Form], Routes);
walk_ast([Form|Rest], Acc, Routes) ->
    walk_ast(Rest, Acc ++ [Form], Routes).

%% -----------------------------------------------------------------------------
%% check functions' head
%% -----------------------------------------------------------------------------
check_clauses({function, _, Method, 3, Clauses}) ->
    %% collect routes
    F = fun({clause, _, E, _, _}) ->
                %% e.g. get("/", _Req, _State)
                {string, _, Route} = hd(E),
                %% e.g. {"/", <<"GET">>}
                {Route, http_method(Method)}
        end,
    [F(Clause) || Clause <- Clauses].

http_method(get) -> <<"GET">>;
http_method(put) -> <<"PUT">>;
http_method(post) -> <<"POST">>;
http_method(delete) -> <<"DELETE">>.
