-module(refac_list_matched_bin_wrapper).
-behaviour(gen_composite_refac).

-include("../include/wrangler.hrl").

-export([composite_refac/1, input_par_prompts/0, select_focus/1]).

-compile([export_all]).

%%------------------------------------------------------------------------------
%% Required

input_par_prompts() -> 
    [].

select_focus(_Args=#args{current_file_name = File, cursor_pos = Pos}) ->
    case api_interface:pos_to_fun_def(File, Pos) of
	{ok, FunDef} ->
	    case api_interface:pos_to_fun_name(File, Pos) of
		{ok, {M, F, A, _, _}} ->
		    {ok, {{M, F, A}, FunDef}};
		{error, Msg} ->
		    {error, Msg}
	    end;
	{error, Msg} ->
	    {error, Msg}
    end.

composite_refac(_Args=#args{current_file_name = File, 
			    focus_sel = {{M, F, A}, FunDef}}) ->
    {refactoring, refac_force_remove_arg, [File, a]}.
    %% ?interactive([?refac_({refactoring, refac_make_list_arg_generic, [a]}, [a])]).

%%------------------------------------------------------------------------------
%% Assisting Functions 
