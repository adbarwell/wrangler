-module(refac_workflow_reduce_insert).
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
    [
     "Name of Module containing Reduction Function: ",
     "Name of Reduction Function: ",
     "Name of Module containing Decomposition Function: ",
     "Name of Decomposition Function: "
    ].

-spec select_focus(_Args::#args{}) -> {ok, syntaxTree()} | {ok, none}.
select_focus(_Args=#args{current_file_name = File,
			 highlight_range = {Start, End}}) ->
    api_interface:pos_to_expr1(File, Start, End).

-spec check_pre_cond(_Args::#args{}) -> ok | {error, string()}.
check_pre_cond(_Args=#args{current_file_name = File,
			  user_inputs = [RMod, RFun, DMod, DFun]}) ->
    checkArity(File, RMod, RFun, DMod, DFun).

-spec selective() -> boolean().
selective() ->
    true.

-spec transform(_Args::#args{}) -> {ok, syntaxTree()} | {error, string()}.
transform(_Args=#args{current_file_name = File,
		      focus_sel = Expr,
		      user_inputs = [RMod, RFun, DMod, DFun]}) ->
    ?FULL_TD_TP([workflow(File, Expr, RMod, RFun, DMod, DFun)], [File]).

%%------------------------------------------------------------------------------
%% Rules

workflow(File, Expr, RMod, RFun, DMod, DFun) ->
    ?RULE(
       ?T("[Tuples@@@]"),
       begin
	   ?TO_AST(lists:concat(["[", ?PP(Tuples@@@), ", {reduce, fun ", 
				 replaceModIfCurrent(File, RMod), ":", RFun, 
				 "/2, fun ", replaceModIfCurrent(File, DMod), 
				 ":", DFun, "/1}]"]))
       end,
       begin
	   Matched = checkValidSkeletons(Tuples@@@),
	   case Matched of
	       true ->
		   locationCheck(Expr, _This@);
	       false ->
		   false
	   end
       end).

%%------------------------------------------------------------------------------
%% Assisting Functions

-spec locationCheck(syntaxTree(), syntaxTree()) -> boolean().
%% @doc Limits the refactoring to only the selected instance.
locationCheck(Expr, _This@) ->
    api_refac:start_end_loc(_This@) == api_refac:start_end_loc(Expr).

-spec checkValidSkeletons(list(syntaxTree())) -> boolean().
%% @doc Determines whether a given list of syntaxTrees is a list of pipeline 
%%      tuples.
checkValidSkeletons([]) ->
    true;
checkValidSkeletons([Tuple | Tuples]) ->
    Matching = ?MATCH(?T("{seq, SeqRest@@@}"), Tuple) or 
	?MATCH(?T("{farm, FarmRest@@@}"), Tuple) or
	?MATCH(?T("{ord, OrdRest@@@}"), Tuple) or
	?MATCH(?T("{reduce, RedRest@@@}"), Tuple) or
	?MATCH(?T("{map, MapRest@@@}"), Tuple) or
	?MATCH(?T("{cluster, ClustRest@@@}"), Tuple) or
	?MATCH(?T("{feedback, FBackRest@@@}"), Tuple),
    case Matching of
    	true ->
                checkValidSkeletons(Tuples);
    	false ->
    	    false
    end.

-spec checkArity(string(), string(), string(), string(), string()) -> 
			ok | {error, string()}.
%% @doc Enquires about Reduction and Decomposition functions with the aim of 
%%      discovering existence and arity if possible.
%%      
%%      NB Standard checkArity caveats apply.
checkArity(File, RMod, RFun, DMod, DFun) ->
    {ok, CMod} = api_refac:module_name(File),
    case checkFun(File, RMod, RFun, atom_to_list(CMod), 2) of
	ok ->
	    checkFun(File, DMod, DFun, atom_to_list(CMod), 1);
	{error, Msg} ->
	    {error, Msg};
	_else ->
	    {error, "refac_workflow_reduce_insert:checkArity/4 " ++ 
		 "has stumbled across something strange."}
    end.

-spec checkFun(string(), string(), string(), 
	       string(), non_neg_integer()) -> ok | {error, string()}.
%% @doc Checks the given function for existence and arity where possible.
checkFun(File, Mod, Fun, CMod, Arity) ->
    case CMod == Mod of
	true ->
	    Funs = api_refac:inscope_funs(File),
	    checkModFunArity(list_to_atom(Mod), list_to_atom(Fun), Funs, Arity);
	false ->
	    ok
    end.

-spec checkModFunArity(atom(), atom(), 
		       [{atom(), atom(), integer()}], non_neg_integer()) ->
			      ok | {error, string()}.
%% @doc Check for a function Fun in module Mod with an arity of Arity.
checkModFunArity(Mod, Fun, [], Arity) ->
    {error, lists:concat(["Could not find ", Mod, ":", Fun, "/", Arity,
			  " in current module; function must take ", Arity,
			  " argument."])};
checkModFunArity(Mod, Fun, [{Mod, Fun, Arity} | _], Arity) ->
    ok;
checkModFunArity(Mod, Fun, [_|Rest], Arity) ->
    checkModFunArity(Mod, Fun, Rest, Arity).

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
