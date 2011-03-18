%%
%% Range search
%%
%% This small library implements a proof-of-concept of a binary search
%% tree where you can specify the keys as a range between two
%% values. The search value may be any value that falls within this
%% range.
-module(tree_range).
-author('knutin@gmail.com').

-include_lib("eunit/include/eunit.hrl").

-export([from_orddict/1, search/2, sample_data/0]).

-record(node, {key, val, left, right}).

sample_data() ->
    %% {{from, to}, value}
    orddict:from_list([{{1, 4}, 1},
                       {{5, 9}, 2},
                       {{10, 14}, 3},
                       {{15, 19}, 4},
                       {{20, 25}, 5}
                      ]).

from_orddict([]) ->
    undefined;

from_orddict([{Key, Val}]) ->
    #node{key = Key, val = Val};

from_orddict(Levels) ->
    {Left, Right} = lists:split(length(Levels) div 2, Levels),
    [{Key, Val} | RightRest] = Right,

    #node{key = Key, val = Val,
          left = from_orddict(Left), right = from_orddict(RightRest)}.    


search(X) ->
    search(X, from_orddict(sample_data())).

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


search_test_() ->
    [
     ?_assertEqual(1, search(1)),
     ?_assertEqual(1, search(3)),
     ?_assertEqual(1, search(4)),

     ?_assertEqual(2, search(5)),
     ?_assertEqual(2, search(9)),
     ?_assertEqual(5, search(22)),

     ?_assertEqual(not_found, search(0)),
     ?_assertEqual(not_found, search(100))
    ].
