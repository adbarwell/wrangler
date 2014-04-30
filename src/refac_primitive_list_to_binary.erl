-module(refac_primitive_list_to_binary).

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
    ?FULL_TD_TP([std_list(File, Expr)], [File]).

%%------------------------------------------------------------------------------
%% Rules 

std_list(File, Expr) ->
    ?RULE(?T("N@ = Lst@@"),
	  begin
	      ?TO_AST("N@ = list_to_binary(Lst@@)")
	  end,
	  begin
	      case locationCheck(Expr, _This@) of
		  true ->
		      typing(File, Expr, Lst@@, N@),
		      true;
		  _ ->
		      false
	      end
	  end).

%%------------------------------------------------------------------------------
%% Assisting Functions 

-spec locationCheck(syntaxTree(), syntaxTree()) -> boolean().
locationCheck(Expr, _This@) ->
    api_refac:start_end_loc(_This@) == api_refac:start_end_loc(Expr).

typing(File, Expr, Lst@@, N@) ->
    Ftypes = ntyper:rshow(File),
    ?print(Ftypes).
