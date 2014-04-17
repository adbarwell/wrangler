-module(test).
-compile([export_all]).

-spec fib(non_neg_integer()) -> non_neg_integer().
fib(0) ->
    0;
fib(1) ->
    1;
fib(X) ->
    fib(X-1) + fib(X-2).

-spec id(any()) -> any().
id(X) ->
    X.

-spec false(any()) -> boolean().
false(_) ->
    false.

-spec plus(integer(), integer()) ->  integer().
plus(X, Y) ->
    X + Y.

succ(X) ->
    X + 1.

-spec input() -> [integer()].
input() ->
    lists:seq(1, 100).

-spec run() -> any().
run() ->
    skel:do([{farm, [{seq,fun(X) -> X end}], 8}], lists:seq(1, 500)).

-spec alt() -> any().
alt() ->
    fib(25),
    id(lists:seq(1,100)),
    hd(skel:do([{seq, fun other:id/1}], [lists:seq(1,100)])).

-spec test() -> boolean().
test() ->
    alt() == id(lists:seq(1,100)).

%%------------------------------------------------------------------------------
%% Individual workflow elements 

-spec seq() -> ok.
seq() ->
    fib(25),
    lists:last(input()),
    ok.

-spec seqS() -> ok.
seqS() ->
    skel:do([{seq, fun (X) -> X end}, {seq, fun ?MODULE:id/1},
             {seq, fun other:id/1}], input()),
    skel:do([{seq, fun ?MODULE:fib/1}, {seq, fun ?MODULE:succ/1}], input()),
    skel:do([{seq, fun ?MODULE:false/1}], input()),
    ok.

pipeS() ->
    skel:do([{pipe, [{seq, fun ?MODULE:id/1}]}], input()),
    skel:do([{pipe, [{seq, fun ?MODULE:false/1}, {seq, fun ?MODULE:id/1}]}], input()),
    skel:do([{farm, [{pipe, {ord, [{cluster, [{seq, fun ?MODULE:id/1}], fun ?MODULE:id/1, fun ?MODULE:id/1}]}}], 8}], input()),
    skel:do([{pipe, [{seq, fun ?MODULE:id/1}]}, {pipe, [{seq, fun ?MODULE:id/1}]}], input()),
    ok.

-spec farmS() -> ok.
farmS() ->
    skel:do([{farm, [{seq, fun ?MODULE:id/1}], 8}], input()),
    skel:do([{farm, [{seq, fun ?MODULE:id/1}], 8}, {farm, [{seq, fun ?MODULE:id/1}], 10}], input()),
    ok.

-spec ordS() -> ok.
ordS() ->
    skel:do([{ord, [{farm, [{seq, fun ?MODULE:id/1}], 8}]}, {ord, [{seq, fun ?MODULE:id/1}]}], input()),
    skel:do([{ord, [{seq, fun ?MODULE:id/1}]}], input()),
    ok.

-spec mapS() -> ok.
mapS() ->
    skel:do([{map, [{seq, fun ?MODULE:id/1}]}], lists:duplicate(10, input())),
    skel:do([{map, [{seq, fun ?MODULE:id/1}]}, {map, [{seq, fun ?MODULE:id/1}], 8}], lists:duplicate(10, input())),
    ok.

-spec clusterS() -> ok.
clusterS() ->
    skel:do([{cluster, [{seq, fun ?MODULE:id/1}], fun ?MODULE:id/1, fun ?MODULE:id/1}, {cluster, [{seq, fun ?MODULE:id/1}], fun ?MODULE:id/1, fun ?MODULE:id/1}], lists:duplicate(10, input())),
    ok.

-spec reduce() -> ok.
reduce() ->
    lists:foldl(fun(X, Acc) -> X + Acc end, 0, lists:seq(1, 5)),
    lists:sum(input()),
    ok.

-spec reduceTest() -> boolean().
reduceTest() ->
    lists:sum(input()) == hd(skel:do([{reduce, fun (X, Y) -> X + Y end,
                                       fun (X) -> X end}],
                                     [input()])).

-spec reduceS() -> ok.
reduceS() ->
    skel:do([{reduce, fun ?MODULE:plus/2, fun ?MODULE:id/1}], [input()]),
    ok.

-spec feedbackS() -> ok.
feedbackS() ->
    skel:do([{feedback, [{farm, [{seq, fun ?MODULE:id/1}], 8}, {seq, fun other:id/1}], fun ?MODULE:const/1}], input()),
    skel:do([{feedback, [{seq, fun ?MODULE:id/1}], fun ?MODULE:const/1}], input()),
    ok.

%%------------------------------------------------------------------------------
%% Regex helper

-spec rerun(string(), string()) -> {match, [{integer(), integer()}]} | nomatch.
rerun(CStr, ReStr) -> 
    re:run(CStr, ReStr, [global, notempty, {capture, first, list}]).

%%------------------------------------------------------------------------------
%% Debugging

-spec debug() -> ok.
debug() -> 
    {seq, fun(X) -> X end}, {seq, fun other:id/1},
    ok.
