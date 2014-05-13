-module(refac_rec_lst_fun_to_bin).
-behaviour(gen_refac).

-include("../include/wrangler.hrl").

-export([input_par_prompts/0, 
	 select_focus/1, 
	 check_pre_cond/1, 
	 selective/0, 
	 transform/1]).

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
%% Required

-spec input_par_prompts() -> [string()].
input_par_prompts() ->
    ["Index of List argument: "].

-spec select_focus(_Args::#args{}) -> {ok, syntaxTree()} | {ok, none} | none.
%% select_focus(_Args=#args{current_file_name = File,
%% 			 highlight_range = {Start, End}}) ->
    %% api_interface:pos_to_expr1(File, Start, End).
select_focus(_Args=#args{current_file_name = File, cursor_pos = Pos}) ->
    api_interface:pos_to_fun_def(File, Pos).

-spec check_pre_cond(_Args::#args{}) -> ok.
check_pre_cond(Args=#args{current_file_name = File,
			  cursor_pos = Pos,
			  user_inputs = [Index]}) ->
    I = list_to_integer(Index),
    case (I > 0) of
	true ->
	    case api_interface:pos_to_fun_name(File, Pos) of 
		{ok, {_, _, A, _, _}} ->
		    case I =< A of
			true ->
			    ok;
			false ->
			    {error, 
			     "Index given is greater than the function arity"}
		    end;
		{error, Msg} ->
		    {error, Msg}
	    end;
	false ->
	    {error, "Index is less than 1"}
    end.

-spec selective() -> boolean().
selective() ->
    true.

-spec transform(_Args::#args{}) -> {ok, syntaxTree()} | {error, string()}.
transform(Args=#args{current_file_name = File,
		     cursor_pos = Pos,
		     user_inputs = [Index],
		     focus_sel = Expr}) ->
    %% setElement(Args).
    %% ?print(Args),
    %% R = api_interface:pos_to_fun_name(File, Pos),
    %% ?print(R),
    %% ?FULL_TD_TP([
    %% 		 %% setElement(File, Expr)
    %% 		 first(File, Expr)
    %% 		 %% id(File, Expr)
    %% 		], [File]).
    case api_interface:pos_to_fun_name(File, Pos) of 
	{ok, {M, F, A, _, _}} ->
	    ?FULL_TD_TP([
			 %% first(File, Expr, {M, F, A}),
			 second(File, Expr, {M, F, A}, list_to_integer(Index))
			], [File]);
	{error, Msg} ->
	    {error, Msg}
    end.

%%------------------------------------------------------------------------------
%% Rules

first(_File, Expr, FunSig) ->
    ?print(first),
    ?print(Expr),
    ?print(FunSig),
    ?RULE(?T("f@(X@, [], Z@) -> [];"),
	  begin
	      {_, F, _} = FunSig,
	      ?print(F),

	      

	      ASTString = lists:concat([F, "(X@, <<>>, Z@) -> <<>>;"]),
	      ?TO_AST(ASTString)
	  end,
	  begin
	      ?print(f@),
	      true
	      %% R = api_refac:fun_define_info(f@) == FunSig,
	      %% ?print(R),
	      %% R
	  end).

second(_File, Expr, FunSig, Index) ->
    ?print(second),
    ?print(FunSig),
    ?RULE(?T("f@(Args@@) -> Clauses@@;"),
	  begin
	      ?TO_AST(refacClauseType(Args@@, Clauses@@, Expr, Index))
	  end,
	  begin
	      functionCheck(f@, length(Args@@), FunSig)
	      %% locationCheck(Expr, _This@)
	  end).

id(_File, _Expr) ->
    X = ?RULE(?T("id(X@) -> X@"),
	  begin
	      ?TO_AST("id(X@)")
	  end,
	      true),
    ?print(X),
    X.

%%------------------------------------------------------------------------------
%% Assisting Functions 

-spec locationCheck(syntaxTree(), syntaxTree()) -> boolean().
%% @doc Limits the refactoring to only the selected instance.
locationCheck(Expr, _This@) ->
    api_refac:start_end_loc(_This@) == api_refac:start_end_loc(Expr).

-spec functionCheck(syntaxTree(), syntaxTree(), functionSig()) -> boolean().

functionCheck({tree, atom, _, F}, A, {_, F, A}) ->
    true;
functionCheck(_, _, _) ->
    false.


-spec refacClauseType(syntaxTree(), 
		      syntaxTree(), syntaxTree(), integer()) -> string().

refacClauseType(Args, Body, Expr, Index) ->
    ?print(Args),
    ?print(Body),
    ?print(Index),
    %% R1 = ?MATCH(?T("X@@, [], Z@@"), Args),
    case ?MATCH(?T("X@@, [], Z@@"), Args) of
	true ->
	    baseCase(?PP(X@@), ?PP(Z@@));
	false ->
	    case ?MATCH(?T("A@@, [_ | R@@], C@@"), Args) of
		true ->
		    replaceCase(?PP(A@@), ?PP(R@@), ?PP(C@@), Body);
		false ->
		    case ?MATCH(?T("E@@, [H@ | R@], F@@"), Args) of 
			true ->
			    recCase(E@@, H@, R@, F@@, Args, Body, Expr);
			false ->
			    {error, 
			     "Possibilities are endless, but this implementation isn't"}
		    end
	    end
    end.

baseCase([], []) ->
    ?print(0),
    "f@(<<>>) -> <<>>;";
baseCase([], Z) ->
    ?print(1),
    ?print(Z),
    "f@(<<>>, " ++ Z ++ ") -> <<>>;";
baseCase(X, []) ->
    ?print(2),
    ?print(X),
    "f@(" ++ X ++ ", <<>>) -> <<>>;";
baseCase(X, Z) ->
    ?print(3),
    ?print(X),
    ?print(Z),
    "f@(" ++ X ++ ", <<>>, " ++ Z ++ ") -> <<>>;".

replaceCase([], R, [], _) ->
    ?print(0),
    ?print(R),
    "f@(<<_, " ++ R ++ "/binary>>) -> " ++ R ++ ";";
replaceCase(A, R, C, _) ->
    ?print(1),
    ?print(A),
    ?print(R),
    ?print(C),
    "f@(" ++ A ++ ", <<_, " ++ R ++ "/binary>>, " ++ C ++ ") -> <<" ++ C ++ 
	", " ++ R ++ "/binary>>;";
replaceCase(_, _, _, _) ->
    ?print(-1),
    "fun(X) -> X end".

recCase(E, H, R, F, Args, [Body], Expr) ->
    ?print(E),
    ?print(H),
    ?print(R),
    ?print(F),
    ?print(Args),
    ?print(Body),
    NewArgs = ?PP(E) ++ ", <<" ++ ?PP(H) ++ ", " ++ ?PP(R) ++ "/binary>>, " ++ 
	?PP(F),
    ?print(NewArgs),
    ?print(api_refac:bound_var_names(Expr)),
    ?print(Body),
    ?print(?PP(Body)),
    case ?MATCH(?T("[H@ | F@(Args@@)]"), Body) of
    	true ->
	    NewBody = "IB = " ++ ?PP(F@) ++ "(" ++ ?PP(Args@@) ++ "), <<" ++ ?PP(H) ++ ", IB/binary>>",
	    ?print(NewBody),
	    NewFull = "f@(" ++ NewArgs ++ ") -> " ++ NewBody ++ ";",
	    ?print(NewFull),
	    NewFull;
    	false ->
    	    ?print(false)
    end.
