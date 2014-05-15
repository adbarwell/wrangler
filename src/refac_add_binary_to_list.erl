-module(refac_add_binary_to_list).
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

-spec select_focus(_Args::#args{}) -> {ok, syntaxTree()} | {ok, none}.
select_focus(_Args=#args{current_file_name = File,
			 cursor_pos = Pos,
			 highlight_range = {Start, End}}) ->
    ?print(Start),
    ?print(End),
    %% VarName = api_interface:pos_to_var_name(File, Pos),
    %% ?print(VarName),
    {ok, {api_interface:pos_to_expr1(File, Start, End), Pos}}.

-spec check_pre_cond(_Args::#args{}) -> ok.
check_pre_cond(_Args) ->
    ok.

-spec selective() -> boolean().
selective() ->
    true.

-spec transform(_Args::#args{}) -> {ok, syntaxTree()} | {error, string()}.
transform(_Args=#args{current_file_name = File,
		      focus_sel = {Expr, Pos}}) ->
    ?FULL_TD_TP([
		 bin_to_lst(File, Expr, Pos)
		], [File]).

%%------------------------------------------------------------------------------
%% Rules

bin_to_lst(_File, Expr, Pos) -> 
    ?print(bin_to_lst),
    ?RULE(
       ?T("f@"),
       begin
	   ?TO_AST("list_to_binary(Bin)")
       end,
       begin
	   ?print(?PP(Expr)),
	   Var = api_interface:pos_to_var(Expr, Pos),
	   ?print(Var),
	   locationCheck(Expr, _This@)
       end).

%%------------------------------------------------------------------------------
%% Assisting Functions 

-spec locationCheck(syntaxTree(), syntaxTree()) -> boolean().
%% @doc Limits the refactoring to only the selected instance.
locationCheck(Expr, _This@) ->
    api_refac:start_end_loc(_This@) == api_refac:start_end_loc(Expr).
