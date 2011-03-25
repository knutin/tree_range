%%
%% Range search in binary search tree
%%
%% This small library implements a binary search tree where the keys
%% is a range between two values. The search value may be any value
%% that falls within this range. The result is a data structure that
%% allows you to very efficiently store large and potentially infinite
%% ranges.
%%
%% Using a dictionary would allow constant lookups at the cost of
%% storing every possible value.
%%
%% As a side note, since any term in Erlang may be compared to another
%% term, the range values may be whatever. This is especially useful
%% for expressing infinity. See sample_data/0 for an example.
%%
%%
%% Copyright (c) 2011 Knut Nesheim knutin@gmail.com
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
-module(tree_range).
-author('knutin@gmail.com').

-include_lib("eunit/include/eunit.hrl").

-export([from_orddict/1, search/2, sample_data/0]).
-export([run_bench/0, do_run_bench/3]).

-type key()   :: {Low::term(), High::term()}.
-type value() :: term().

-record(node, {key   :: key(),
               val   :: value(),
               left  :: tree(),
               right :: tree()
}).

-type tree() :: {#node{}}.


-spec from_orddict([{key(), value()}]) -> tree().
%% @doc: Converts an ordered dictionary as returned from
%% orddict:to_list/1 into a tree. The key must be a {Low, High} tuple.
from_orddict([]) ->
    undefined;

from_orddict([{Key, Val}]) ->
    #node{key = Key, val = Val};

from_orddict(Levels) ->
    {Left, Right} = lists:split(length(Levels) div 2, Levels),
    [{Key, Val} | RightRest] = Right,

    #node{key = Key, val = Val,
          left = from_orddict(Left), right = from_orddict(RightRest)}.


-spec search(X::term(), Tree::tree()) -> Value::value() | 'not_found'.

%% Within range, return the value
search(X, #node{key = {Lo, Hi}} = Node) when Lo =< X andalso X =< Hi ->
    Node#node.val;
%% Below range, traverse left
search(X, #node{key = {Lo, _Hi}} = Node) when X < Lo ->
    search(X, Node#node.left);
%% Above range, traverse right
search(X, #node{key = {_Lo, Hi}} = Node) when Hi < X ->
    search(X, Node#node.right);
%% Hit bottom
search(_X, undefined) ->
    not_found.


%%
%% TESTS
%%

sample_data() ->
    orddict:from_list([
                       {{0, 4}, 1},
                       {{5, 16}, 2},
                       {{17, 31}, 3},
                       {{32, 66}, 4},
                       {{67, 101}, 5},
                       {{102, 151}, 6},
                       {{152, 195}, 7},
                       {{196, 253}, 8},
                       {{254, 322}, 9},
                       {{323, 399}, 10},
                       {{400, 487}, 11},
                       {{488, 586}, 12},
                       {{587, 691}, 13},
                       {{692, 806}, 14},
                       {{807, 931}, 15},
                       {{932, 1066}, 16},
                       {{1067, 1211}, 17},
                       {{1212, 1366}, 18},
                       {{1367, 1531}, 19},
                       {{1532, 1706}, 20},
                       {{1707, 1891}, 21},
                       {{1892, 2087}, 22},
                       {{2088, 2293}, 23},
                       {{2294, 2509}, 24},
                       {{2510, 2736}, 25},
                       {{2737, 2981}, 26},
                       {{2982, 3237}, 27},
                       {{3238, 3512}, 28},
                       {{3513, 3797}, 29},
                       {{3798, 4097}, 30},
                       {{4098, 4407}, 31},
                       {{4408, 4732}, 32},
                       {{4733, 5077}, 33},
                       {{5078, 5442}, 34},
                       {{5443, 5827}, 35},
                       {{5828, 6232}, 36},
                       {{6233, 6657}, 37},
                       {{6658, 7107}, 38},
                       {{7108, 7582}, 39},
                       {{7583, 8082}, 40},
                       {{8083, 8607}, 41},
                       {{8608, 9157}, 42},
                       {{9158, infinity}, 43}]).

sample_data_search_test() ->
    SampleData = sample_data(),
    Tree = from_orddict(SampleData),

    %% Generates an assert for every possible input value that falls
    %% within the ranges in SampleData
    lists:map(fun({Ints, Value}) ->
                      [?assertEqual(Value, search(Int, Tree)) || Int <- Ints]
              end,
              lists:map(fun ({{Lo, infinity}, Value}) ->
                                {lists:seq(Lo, Lo + 10), Value};
                            ({{Lo, Hi}, Value}) ->
                                {lists:seq(Lo, Hi), Value}
                        end,
                        SampleData)).

search_test_() ->
    Tree = from_orddict(sample_data()),
    [
     ?_assertEqual(not_found, search(-1, Tree)),
     ?_assertEqual(1, search(0, Tree)),
     ?_assertEqual(43, search(100000000, Tree)),
     ?_assertEqual(43, search(infinity, Tree)),
     ?_assertEqual(43, search(foobar, Tree)),
     ?_assertEqual(not_found, search([], Tree)),
     ?_assertEqual(not_found, search({}, Tree))
    ].


%%
%% Benchmarks
%%
%% R:
%% Data <- read.csv("results.csv")
%% summary(Data)
%% plot(Data)

run_bench() ->
    {Min, Max} = {1, 10000},

    Tree = tree_range:from_orddict(tree_range:sample_data()),

    Data = [{N, run_bench(N, Tree)} || N <- lists:seq(Min, Max)],
    Data2 = [{"key", "time"} | Data],

    file:write_file("results.csv",
                    [io_lib:format("~p,~p~n", [A, B]) || {A, B} <- Data2]),

    ok.

run_bench(N, Tree) ->
    {T, _} = timer:tc(?MODULE, do_run_bench, [1000, N, Tree]),
    T.

do_run_bench(0, _N, _Tree) ->
    ok;

do_run_bench(Rep, N, Tree) ->
    tree_range:search(N, Tree),
    do_run_bench(Rep-1, N, Tree).
