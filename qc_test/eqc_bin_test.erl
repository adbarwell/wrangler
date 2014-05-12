-module(eqc_bin_test).
-compile([export_all]).

-include_lib("eqc/include/eqc.hrl").

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
%% Worker functions

%% -spec setElement(pos_integer(), [bin_int()], bin_int()) -> [bin_int()]. 
setElement(_I, [], _New) -> 
    [];
setElement(1, [_|Rest], New) -> 
    [New|Rest];
setElement(I, [E|Rest], New) -> 
    [E | setElement(I-1, Rest, New)].

%% -spec setElementB(pos_integer(), binary(), bin_int()) -> binary().
setElementB(_I, <<>>, _New) ->
    <<>>;
setElementB(1, <<_, Rest/binary>>, New) ->
    <<New, Rest/binary>>;
setElementB(I, <<E, Rest/binary>>, New) ->
    IB = setElementB(I-1, Rest, New),
    <<E, IB/binary>>.

bconcat(B1, B2) ->
    <<B1/binary, B2/binary>>.

%%------------------------------------------------------------------------------
%% Checking utilities 

check(I, Lst, N) ->
    R1 = setElement(I, Lst, N),
    R1b = binary_to_list(list_to_binary(R1)),
    R1 =:= R1b.

check_2(I, Lst, N) ->
    R1 = setElement(I, Lst, N),
    R2 = setElementB(I, list_to_binary(Lst), N),
    R1 =:= binary_to_list(R2). 

gen_lst(Len) when Len > 0 ->
    noshrink(gen_lst(Len, [])).

gen_lst(0, Acc) ->
    Acc;
gen_lst(Len, Acc) ->
    gen_lst(Len-1, [eqc_gen:choose(0, 255) | Acc]).

test_1() ->
    eqc:quickcheck(numtests(10,
			    ?FORALL({I, Lst, N}, 
				    noshrink(resize(256, {choose(1,256), 
				     gen_lst(255), choose(0, 255)})),
				    check(I, Lst, N)))).

test_2() ->
    eqc:quickcheck(numtests(1000,
			    ?FORALL({I, Lst, N},
				    noshrink({choose(1,256), 
					      gen_lst(255), choose(0, 255)}),
				    check_2(I, Lst, N)))).

run() ->
    test_1(),
    test_2().
