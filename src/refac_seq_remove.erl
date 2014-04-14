-module(refac_seq_remove).
-behaviour(gen_refac).
-include("../include/wrangler.hrl").

-export([input_par_prompts/0, 
	 select_focus/1, 
	 check_pre_cond/1,
	 selective/0,
	 transform/1]).

-compile([export_all]).


-spec input_par_prompts() -> [string()].
input_par_prompts() ->
    [].

-spec select_focus(Args::#args{}) -> {ok, syntaxTree()} | {ok, none}.
select_focus(_Args=#args{current_file_name = File,
			 highlight_range = {Start, End}}) ->
    api_interface:pos_to_expr1(File, Start, End).

-spec check_pre_cond(Args::#args{}) -> ok.
check_pre_cond(_) ->
    ok.

-spec selective() -> boolean().
selective() ->
    true.

-spec transform(Args::#args{}) -> {ok, syntaxTree()} | {error, string()}.
transform(_Args=#args{current_file_name = File,
		      focus_sel = Expr}) ->
    ?FULL_TD_TP([reverse_fun_in_mod(Expr), reverse_fun_call(Expr)], [File]).


remove_hd(Expr) ->
    ?RULE(?T("hd(Rest@@@)"), ?TO_AST(?PP(Rest@@@)), locationCheck(Expr, _This@)).

reverse_fun_in_mod(Expr) ->
    ?RULE(
       ?T("hd(skel:do([{seq, fun ?MODULE:f@/1}], [Input@]))"),
       begin
	   ?TO_AST("f@(Input@)")
       end,
       locationCheck(Expr, _This@)).

reverse_fun_call(Expr) ->
    ?RULE(?T("hd(skel:do([{seq, fun m@:f@/1}], [Input@]))"), 
	  ?TO_AST("m@:f@(Input@)"), 
	  locationCheck(Expr, _This@)).


-spec locationCheck(syntaxTree(), syntaxTree()) -> boolean().
locationCheck(Expr, _This@) ->
    api_refac:start_end_loc(_This@) == api_refac:start_end_loc(Expr).
