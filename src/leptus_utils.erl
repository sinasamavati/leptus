%% The MIT License

%% Copyright (c) 2013-2014 Sina Samavati <sina.samv@gmail.com>

%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:

%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.

%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.

-module(leptus_utils).

%% -----------------------------------------------------------------------------
%% API
%% -----------------------------------------------------------------------------
-export([priv_dir/1]).
-export([paginator/1]).
-export([paginate/3]).

%% -----------------------------------------------------------------------------
%% find the path to the priv directory in an application
%% -----------------------------------------------------------------------------
priv_dir(App) ->
    case code:priv_dir(App) of
        {error, bad_name} ->
            Ebin = filename:dirname(code:which(App)),
            filename:join(filename:dirname(Ebin), "priv");
        PrivDir ->
            PrivDir
    end.

%% -----------------------------------------------------------------------------
%% generate a paginator
%% -----------------------------------------------------------------------------
-spec paginator(non_neg_integer()) -> fun((non_neg_integer(), list()) -> list()).
paginator(NElemPerPage) ->
    fun(Page, Objects) ->
            paginate(NElemPerPage, Objects, Page)
    end.

%% -----------------------------------------------------------------------------
%% paginate a list of objects
%% -----------------------------------------------------------------------------
-spec paginate(non_neg_integer(), list(), non_neg_integer()) -> list().
paginate(NElemPerPage, Objects, Page) ->
    Last = Page * NElemPerPage,
    Start = Last - NElemPerPage,
    Len = Last - Start,
    lists:sublist(Objects, Start + 1, Len).
