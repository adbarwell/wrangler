-module(refac_workflow_seq_insert).
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
     "Name of Module containing Function: ", 
     "Name of Function: ",
     "Index of Skeleton to Insert After (0 prepends to the workflow): "
    ].

-spec select_focus(_Args::#args{}) -> {ok, syntaxTree()} | {ok, none}.
select_focus(_Args=#args{current_file_name = File,
			 highlight_range = {Start, End}}) ->
    api_interface:pos_to_expr1(File, Start, End).

-spec check_pre_cond(_Args::#args{}) -> ok.
check_pre_cond(_Args=#args{current_file_name = File,
			    user_inputs = [Mod, Fun, _Pos]}) ->
    checkArity(File, Mod, Fun).

-spec selective() -> boolean().
selective() ->
    true.

-spec transform(_Args::#args{}) -> {ok, syntaxTree()} | {error, string()}.
transform(_Args=#args{current_file_name=File, 
		      focus_sel=Expr,
		      user_inputs=[Mod, Fun, Pos]}) ->
    ?FULL_TD_TP([insert(File, Expr, Mod, Fun, Pos)], [File]).

%%------------------------------------------------------------------------------
%% Rules 

insert(File, Expr, Mod, Fun, RawPos) ->
    ?RULE(
       ?T("[Tuples@@@]"),
       begin
	   {Pos, _} = string:to_integer(RawPos),
	   ModStr = replaceModIfCurrent(File, Mod),
	   ?TO_AST(insertSeq(Tuples@@@, ModStr, Fun, Pos))
       end,
       begin
	   Matched = checkValidSkeletons(Tuples@@@),
	   case Matched of
	       true ->
		   locationCheck(Expr, _This@) and 
		       checkOoBIndex(RawPos, length(Tuples@@@));
	       false ->
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

-spec checkOoBIndex(string(), integer()) -> boolean().
%% @doc Checks if the given index is out of bounds.
checkOoBIndex(Pos, NoTuples) ->
    {IPos, _} = string:to_integer(Pos),
    IPos =< NoTuples.

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

-spec insertSeq([syntaxTree()], string(), string(), integer()) -> string().
insertSeq(Tuples, Mod, Fun, 0) ->
    lists:concat(["[{seq, fun ", Mod, ":", Fun, "/1}, ", ?PP(Tuples), "]"]);
insertSeq(Tuples, Mod, Fun, Pos) when Pos == length(Tuples) ->
    lists:concat(["[", ?PP(Tuples), ", {seq, fun ", Mod, ":", Fun, "/1}]"]);
insertSeq(Tuples, Mod, Fun, Pos) ->
    {Head, Tail} = lists:split(Pos, Tuples),
    lists:concat(["[", ?PP(Head), ", {seq, fun ", Mod, ":", Fun, "/1}, ",
		  ?PP(Tail), "]"]).
