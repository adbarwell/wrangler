-module(bin).
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

checkBin(Xs, Colle) when Xs >= 0 ->
    Colle ! {num, Xs},
    <<X>> = <<Xs>>,
    X == Xs;
checkBin(Xs, Colle) when Xs < 0 ->
    Colle ! {num, Xs},
    <<X/signed>> = <<Xs>>,
    X == Xs.

checkBinLst([]) ->
    true;
checkBinLst([X | Rest]) when X < 256, X > -256 ->
    true and checkBinLst(Rest);
checkBinLst([X | Rest]) ->
    false.


eqc_test() ->
    ?FORALL(Xs, list(int()),
	    lists:reverse(lists:reverse(Xs)) == Xs).

eqc_binary(Colle) ->
    ?FORALL(Xs, 
	    noshrink(choose(0, 255)), 
	    checkBin(Xs, Colle)).

eqc_binary_lst() ->
    ?FORALL(Xs, 
	    ?SUCHTHAT(Ys, list(int()), (Ys > -256) and (Ys < 256)), 
	    checkBinLst(Xs)).

run() ->
    Colle = spawn(bin, collector, [[]]),
    eqc:quickcheck(eqc_test()),
    eqc:quickcheck(eqc_binary(Colle)),
    %% eqc:quickcheck(eqc_binary_lst()),
    Colle ! eos.

collector(Xs) ->
    receive
	{num, X} ->
	    collector([X | Xs]);
	eos ->
	    ?print(averages(Xs))
    end.

averages(Xs) ->
    {{max, lists:max(Xs)},
     {min, lists:min(Xs)},
     {mean, lists:sum(Xs)/length(Xs)},
     {mode, mode(Xs)},
     {range, lists:max(Xs)-lists:min(Xs)}}.

mode([]) -> 
    [];
mode(List) when is_list(List) ->
    mode_front(lists:reverse(lists:keysort(2, histograph(List)))).

mode_front([{Item,Freq}|Tail]) ->
    mode_front(Tail, Freq, [Item]).

mode_front([ {Item, Freq} | Tail], Freq, Results) ->
    mode_front(Tail, Freq, [Item]++Results);

mode_front([{_Item,_Freq} |_Tail],_Better, Results) ->
    Results;

mode_front([], _Freq, Results) -> Results.


%% @doc Copied from scutil because I can't get it to compile easily. 
%%      Will try again later.
histograph([]) ->
    [];
histograph(List) when is_list(List) ->
    [Head|Tail] = lists:sort(List),
    histo_count(Tail, Head, 1, []).

histo_count([], Current, Count, Work) ->
     lists:reverse([{Current,Count}]++Work);
histo_count([Current|Tail], Current, Count, Work) ->
    histo_count(Tail, Current, Count+1, Work);
histo_count([New|Tail], Current, Count, Work) ->
    histo_count(Tail, New, 1, [{Current,Count}] ++ Work).
