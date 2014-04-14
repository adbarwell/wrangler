-module(refac_workflow_farm_insert).
-behaviour(gen_refac).

-include("../include/wrangler.hrl").

-export([input_par_prompts/0,
	 select_focus/1,
	 check_pre_cond/1,
	 selective/0,
	 transform/1]).

-compile([export_all]).

%%------------------------------------------------------------------------------
%% Macros

-define(workflowre, "^\s*{\s*(seq|pipe|farm|ord|map|cluster|reduce|feedback)\s*,.+}(\s*{\s*(seq|pipe|farm|ord|map|cluster|reduce|feedback)\s*,.+})*\s*$").


%%------------------------------------------------------------------------------
%% Required 

-spec input_par_prompts() -> [string()].
input_par_prompts() ->
    [
     "Name of Module containing Worker Function: ", 
     "Name of Worker Function: ",
     "Number of Workers: "
    ].

-spec select_focus(Args::#args{}) -> {ok, syntaxTree()} | {ok, none}.
select_focus(_Args=#args{current_file_name = File,
			 highlight_range = {Start, End}}) ->
    api_interface:pos_to_expr1(File, Start, End).

-spec check_pre_cond(Args::#args{}) -> ok.
check_pre_cond(_Args=#args{current_file_name = File,
			    user_inputs = [Mod, Fun, _]}) ->
    checkArity(File, Mod, Fun).

-spec selective() -> boolean().
selective() ->
    true.

-spec transform(Args::#args{}) -> {ok, syntaxTree()} | {error, string()}.
transform(_Args=#args{current_file_name=File, 
		      focus_sel=Expr,
		      user_inputs=[Mod, Fun, NW]}) ->
    ?FULL_TD_TP([insert(File, Expr, Mod, Fun, NW)], [File]).

%%------------------------------------------------------------------------------
%% Rules 

insert(File, Expr, Mod, Fun, NW) ->
    ?RULE(
       ?T("[Tuples@@@]"),
       begin
	   TuplesStr@@@ = ?PP(Tuples@@@),
	   ModStr = replaceModIfCurrent(File, Mod),
	   ?TO_AST("["++?PP(Tuples@@@)++", {farm, [{seq, fun " ++ ModStr ++ ":" ++ Fun ++ "/1}], " ++ NW ++ "}]")
       end,
       begin
	   TuplesStr = ?PP(Tuples@@@),
	   Matched = re:run(TuplesStr, ?workflowre, 
			    [global, notempty, {capture, first, list}]),
	   case Matched of
	       {match, _} ->
		   locationCheck(Expr, _This@);
	       nomatch ->
		   false;
	       _else ->
		   %% io:format("Matched: ~p~n", Matched]),
		   false
	   end
       end).

%%------------------------------------------------------------------------------
%% Assisting Functions 

-spec locationCheck(syntaxTree(), syntaxTree()) -> boolean().
%% @doc Limits the refactoring to only the selected instance.
locationCheck(Expr, _This@) ->
    Start1 = api_refac:start_end_loc(_This@),
    Start2 = api_refac:start_end_loc(Expr),
    Start1 == Start2.

-spec checkArity(string(), string(), string()) -> ok | {error, string()}.
%% @doc Checks the given function has an arity of one when that function is in 
%%      the same module as the Skel call.
%%      
%%      NB Not sure how to check other files -- there's possibly a hack around 
%%      this using the ENV in args and module name, but I'll deal with that 
%%      later. It should also be noted that Erlang doesn't check a given call is 
%%      valid anyway, so why should we? (Because then it's safe...)
checkArity(File, Mod, Fun) ->
    {ok, CMod} = api_refac:module_name(File),
    case atom_to_list(CMod) == Mod of
	true ->
	    Funs = api_refac:inscope_funs(File),
	    checkModFunArity(list_to_atom(Mod), list_to_atom(Fun), Funs);
	false ->
	    ok
    end.

-spec checkModFunArity(atom(), atom(), [{atom(), atom(), integer()}]) ->
			      ok | {error, string()}.
%% @doc Checks for a function Fun in module Mod with an arity of 1.
checkModFunArity(Mod, Fun, []) ->
    {error, 
     "Could not find " ++ atom_to_list(Mod) ++ ":" ++ atom_to_list(Fun) ++ 
	 "/1 in current module; function must take one argument."};
checkModFunArity(Mod, Fun, [{Mod, Fun, 1} | _]) ->
    ok;
checkModFunArity(Mod, Fun, [_ | Rest]) ->
    checkModFunArity(Mod, Fun, Rest).

-spec replaceModIfCurrent(string(), string()) -> string().
%% @doc Replaces the module name with the ?MODULE macro should the entered 
%%      module be the same as that represented by the current file.
replaceModIfCurrent(File, Mod) ->
    {ok, CurrMod} = api_refac:module_name(File),
    case atom_to_list(CurrMod) == Mod of
	true ->
	    "?MODULE";
	false ->
	    Mod
    end.
