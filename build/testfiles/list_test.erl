-module(list_test).

-compile([export_all]).

fib(0) ->
    0;
fib(1) ->
    1;
fib(X) ->
    fib(X-1) + fib(X-2).

test() ->
    X = [1,2,3,4,5, 256],
    io:format("X: ~p~n", [X]),
    build_iolist(10).

test2() ->
    X = lists:duplicate(10, <<1,2,3,4,5>>),
    skel:do([{map, [{seq, fun ?MODULE:fib/1}]}], X).
    

-spec build_iolist(non_neg_integer()) -> iolist().

build_iolist(0) ->
    [];
build_iolist(Len) ->
    build_iolist_1([], Len).

-spec build_iolist_1(iolist(), non_neg_integer()) -> iolist().

build_iolist_1(Lst, 0) ->
    Lst;
build_iolist_1(Lst, Len) ->
    build_iolist_1([rand(256)-1 | Lst], Len-1).

rand() ->
    {X, _Seed} = random:uniform_s(now()),
    X.
rand(UpperLimit) ->
    {X, _Seed} = random:uniform_s(UpperLimit, now()),
    X.

%% iolist() ::

%% {c,list,
%%  [{c,union,
%%    [none,
%%     {c,binary,[8,0],unknown},
%%     none,none,
%%     {c,list,
%%      [any,
%%       {c,union,
%%        [none,
%% 	{c,binary,[8,0],unknown},
%% 	none,none,
%% 	{c,nil,[],unknown},
%% 	none,none,none,none,none],
%%        unknown}],
%%      unknown},
%%     {c,number,{int_rng,0,255},integer},
%%     none,none,none,none],
%%    unknown},
%%   {c,union,
%%    [none,
%%     {c,binary,[8,0],unknown},
%%     none,none,
%%     {c,nil,[],unknown},
%%     none,none,none,none,none],
%%    unknown}],
%%  unknown}
