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
    [].

-spec select_focus(_Args::#args{}) -> {ok, syntaxTree()} | {ok, none} | none.
%% select_focus(_Args=#args{current_file_name = File,
%% 			 highlight_range = {Start, End}}) ->
    %% api_interface:pos_to_expr1(File, Start, End).
select_focus(_Args=#args{current_file_name = File, cursor_pos = Pos}) ->
    api_interface:pos_to_fun_def(File, Pos).

-spec check_pre_cond(_Args::#args{}) -> ok.
check_pre_cond(_) ->

-spec selective() -> boolean().
selective() ->
    true.

-spec transform(_Args::#args{}) -> {ok, syntaxTree()} | {error, string()}.
transform(Args=#args{current_file_name = File,
		     cursor_pos = Pos,
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
			 first(File, Expr, {M, F, A}),
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
	      AstString = ""
	      ?TO_AST(AstString)
	  end,
	  begin
	      true
	  end).

%%------------------------------------------------------------------------------
%% Assisting Functions 
