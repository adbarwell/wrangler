-module(refac_seq_intro).

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
transform(_Args=#args{current_file_name=File, focus_sel=Expr}) ->
    %% Start1 = api_refac:start_end_loc(Expr),
    %% io:format("Start1: ~p~n", [Start1]),
    ?FULL_TD_TP([fun_in_mod(Expr), fun_call(Expr)], [File]).

%%------------------------------------------------------------------------------
%% Rules 

fun_in_mod(Expr) ->
    io:format("fun_in_mod~n"),
    io:format("Expr: ~p~n", [Expr]),
    ?RULE(
       ?T("f@(Input@)"),
       begin
	   ?TO_AST("hd(skel:do([{seq, fun ?MODULE:f@/1}], [Input@]))")
       end,
       begin
	   locationCheck(Expr, _This@)
       end).

fun_call(Expr) ->
    io:format("fun_call~n"),
    ?RULE(
       ?T("m@:f@(Input@)"),
       begin
	   ?TO_AST("hd(skel:do([{seq, fun m@:f@/1}], [Input@]))")
       end,
       begin
	   locationCheck(Expr, _This@)
       end).


%%------------------------------------------------------------------------------
%% Assisting Functions 

-spec locationCheck(syntaxTree(), syntaxTree()) -> boolean().
locationCheck(Expr, _This@) ->
    Start1 = api_refac:start_end_loc(_This@),
    Start2 = api_refac:start_end_loc(Expr),
    Start1 == Start2.
