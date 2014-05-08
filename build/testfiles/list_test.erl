-module(list_test).

-compile([export_all]).


%%------------------------------------------------------------------------------
%% Debugging 

-ifndef(debug).
-define(debug, true).
%% -define(debug, false).
-endif.

-ifndef(print).
-define(print(Var), case ?debug of
			true ->
			    io:format("~p:~p~n  ~p: ~p~n", 
				      [?MODULE, ?LINE, ??Var, Var]);
			false ->
			    ok
		    end).
-endif.

%%------------------------------------------------------------------------------

fib(0) ->
    0;
fib(1) ->
    1;
fib(X) ->
    fib(X-1) + fib(X-2).

id(X) -> X.

test() ->
    X = <<1, 2, 3, 4, 5, 255>>,
    io:format("X: ~p~n", [X]),
    ?print(binary_to_list(X)).
    %% build_iolist(10).

test2() ->
    X = lists:duplicate(10, <<1,2,3,4,5>>),
    skel:do([{map, [{seq, fun ?MODULE:fib/1}]}], X).

img_test() ->
    Input = [[1,2,3,4,4,5,6,6,7,8,8,9,9,0,0,10]],
    skel:do([{map, [{seq, fun ?MODULE:id/1}, {seq, fun ?MODULE:fib/1}]}, {seq, fun ?MODULE:id/1}], Input).
   
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
