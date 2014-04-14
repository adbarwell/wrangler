-module(refac_ord_remove).
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

-spec select_focus(#args{}) -> {ok, syntaxTree()} | {ok, none}.
select_focus(_Args=#args{current_file_name = File,
			 highlight_range = {Start, End}}) ->
    api_interface:pos_to_expr1(File, Start, End).

-spec check_pre_cond(#args{}) -> ok.
check_pre_cond(_Args) ->
    ok.

-spec selective() -> boolean().
selective() ->
    true.

-spec transform(#args{}) -> {ok, syntaxTree()} | {error, string()}.
transform(_Args=#args{current_file_name = File,
		      focus_sel = Expr}) ->
    ?FULL_TD_TP([ord(Expr)], [File]).

%%------------------------------------------------------------------------------
%% Rules 

ord(Expr) ->
    ?RULE(
       ?T("{ord, [Pipe@@@]}"),
       begin
	   ?TO_AST("{pipe, [" ++ ?PP(Pipe@@@) ++ "]}")
       end,
       begin
	   locationCheck(Expr, _This@)
       end).

%%------------------------------------------------------------------------------
%% Assisting Functions 

-spec locationCheck(syntaxTree(), syntaxTree()) -> boolean().
locationCheck(Expr, _This@) ->
    api_refac:start_end_loc(_This@) == api_refac:start_end_loc(Expr).

-spec explodePipe(syntaxTree()) -> string().
explodePipe([E]) ->
    io:format("E: ~p~n", [E]),
    "E";
explodePipe([E | Rest]) ->
    io:format("E: ~p~n", [E]),
    "E" ++ explodePipe(Rest).
