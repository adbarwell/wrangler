-module(refac_bin_wrapper).
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
    [
     "Index of binary argument: ",
     "Function clause: "
    ].

%% -spec select_focus(_Args::#args{}) -> {ok, syntaxTree()} | {ok, none} | none.
select_focus(_Args=#args{current_file_name = File, 
			 user_inputs = [Index, Clause], cursor_pos = Pos}) ->
    ?print(File),
    ?print(Index),
    ?print(Clause),
    ?print(Pos),
    case api_interface:pos_to_fun_def(File, Pos) of
	{ok, FunDef} ->
	    ?print(FunDef),
	    ?print(?PP(FunDef)),
	    case api_interface:pos_to_fun_name(File, Pos) of
		{ok, {M, F, A, _, _}} ->
		    ?print(M),
		    ?print(F),
		    ?print(A),
		    case ?MATCH(?T("f@(Args@@@) when G@@@ -> C@@@"), FunDef) of
			true ->
			    ?print(true),
			    ?print(Args@@@),
			    ?print(length(Args@@@)),
			    Args = lists:nth(list_to_integer(Clause), Args@@@),
			    ?print(?PP(Args)),
			    Arg = lists:nth(list_to_integer(Index), Args),
			    ?print(?PP(Arg)),
			    {ok, {{M, F, A}, FunDef, Arg}};
			    %% {error, "Halt here, please."};
			false ->
			    ?print(false),
			    %% {ok, {{M, F, A}, FunDef}}
			    {error, "Halt here, please."}
		    end;
		{error, Msg} ->
		    {error, Msg}
	    end;
	{error, Msg} ->
	    {error, Msg}
    end.

-spec check_pre_cond(Args::#args{}) -> ok.
%% check_pre_cond(_Args=#args{current_file_name = File,
%% 			   user_inputs = [Index, Clause], focus_sel = Expr}) ->
check_pre_cond(_) ->
    %% ?print(check_pre_cond),
    %% {{M, F, A}, _FunDef, Arg} = Expr,
    %% ?print(M),
    %% ?print(F),
    %% ?print(A),
    %% ?print(Arg),
    ok.

-spec selective() -> boolean().
selective() ->
    true.

-spec transform(_Args::#args{}) -> {ok, syntaxTree()} | {error, string()}.
transform(Args=#args{current_file_name = File,
		     cursor_pos = Pos,
		     focus_sel = Expr}) ->
    case api_interface:pos_to_fun_name(File, Pos) of 
	{ok, {M, F, A, _, _}} ->
	    ?FULL_TD_TP([
			 first(File, Expr, {M, F, A})
			], [File]);
	{error, Msg} ->
	    {error, Msg}
    end.

%%------------------------------------------------------------------------------
%% Rules

first(File, Expr, FunSig) -> 
    ?print(first),
    ?print(Expr),
    ?print(FunSig),
    ?RULE(?T(""),
	  begin
	      AstString = "",
	      ?TO_AST(AstString)
	  end,
	  begin
	      true
	  end).

%%------------------------------------------------------------------------------
%% Assisting Functions 
