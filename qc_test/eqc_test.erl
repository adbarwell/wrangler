-module(eqc_test).
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

prop(Prop) ->
    ?print(Prop),
    true.

collect_fun_locs(AST) ->
    F = fun(T, S) ->
		As = wrangler_syntax:get_ann(T),
		?print(As),
		case lists:keysearch(fun_def, 1, As) of
		    {value, {fun_def, {_Mod, _Fun, _Arity, Pos, DefPos}}} ->
			if DefPos == {0,0} ->
				S;
			   true ->
				[Pos] ++ S
			end;
		    _else ->
			S
		end
	end,
    Locs = lists:usort(api_ast_traverse:fold(F, [], AST)) ++ [{0,0}],
    ?print(Locs),
    Locs.

collect_annotations(AST) ->			  
    F = fun(T, S) ->
		[wrangler_syntax:get_ann(T)] ++ S
	end,
    Anns = api_ast_traverse:fold(F, [], AST),
    ?print(Anns),
    Anns.

gen_file(Files) ->
    oneof(Files).

gen(Files) ->
    ?LET(FileName, (gen_file(Files)), gen_1(FileName, Files)).

gen_1(FileName, Files) ->
    {ok, {AST, _Info}} = wrangler_ast_server:parse_annotate_file(FileName, true, Files, 8),
    noshrink({FileName, oneof(collect_annotations(AST)), Files, 8}).

test(Files) ->
    application:start(wrangler),
    eqc:quickcheck(numtests(1, 
			    ?FORALL(C, (gen(Files)), prop(C)))),
			    %% ?FORALL(Xs, list(int()), 
			    %% 	    lists:reverse(lists:reverse(Xs)) == Xs))),
    application:stop(wrangler).

test_1() ->
    test(["/Users/papillon/git/st_andrews/tfp14-wrangler/build/testfiles/test_eqc.erl"]).


run() ->
    test_1().
