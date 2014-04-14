-module(refac_reduce_remove).
-behaviour(gen_refac).

-include("../include/wrangler.hrl").

-export([input_par_prompts/0,
	 select_focus/1,
	 check_pre_cond/1,
	 selective/0,
	 transform/1]).

-compile([export_all]).

%%------------------------------------------------------------------------------
%% Required 

-spec input_par_prompts() -> [string()].
input_par_prompts() ->
    [].

-spec select_focus(_Args::#args{}) -> {ok, syntaxTree()} | {ok, none}.
select_focus(_Args=#args{current_file_name = File,
			 highlight_range = {Start, End}}) ->
    api_interface:pos_to_expr1(File, Start, End).

-spec check_pre_cond(_Args::#args{}) -> ok.
check_pre_cond(_Args) ->
    ok.

-spec selective() -> boolean().
selective() ->
    true.

-spec transform(_Args::#args{}) -> {ok, syntaxTree()} | {error, string()}.
transform(_Args=#args{current_file_name = File,
		      focus_sel = Expr}) ->
    ?FULL_TD_TP([
		 remove_sum(Expr)
		 %% foldl(File, Expr, Mod, Fun), 
		 %% foldr(File, Expr, Mod, Fun)
		], [File]).

%%------------------------------------------------------------------------------
%% Rules

remove_sum(Expr) -> 
    ?RULE(
       ?T("hd(skel:do([{reduce, fun(X, Y) -> X + Y end, fun(X) -> X end}], [Input@@]))"),
       begin
	   ?TO_AST("lists:sum(Input@@)")
       end,
       begin
	   locationCheck(Expr, _This@)
       end).

%%------------------------------------------------------------------------------
%% Assisting Functions

-spec locationCheck(syntaxTree(), syntaxTree()) -> boolean().
%% @doc Limits the refactoring to only the selected instance.
locationCheck(Expr, _This@) ->
    api_refac:start_end_loc(_This@) == api_refac:start_end_loc(Expr).
