-module(refac_workflow_remove).
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
    [
     "Index of Element to Remove: "
    ].

-spec select_focus(_Args::#args{}) -> {ok, syntaxTree()} | {ok, none}.
select_focus(_Args=#args{current_file_name = File,
			 highlight_range = {Start, End}}) ->
    api_interface:pos_to_expr1(File, Start, End).

-spec check_pre_cond(_Args::#args{}) -> ok | {error, string()}.
check_pre_cond(_Args=#args{current_file_name = _File,
			    user_inputs = [RawPos]}) ->
    case string:to_integer(RawPos) of
	{error, Err} ->
	    {error, Err};
	{Pos, _} when is_integer(Pos) ->
	    case Pos > 0 of
		true ->
		    ok;
		false ->
		    {error, 
		     "Given posisition occurs before the start of the pipeline"}
	    end
    end.

-spec selective() -> boolean().
selective() ->
    true.

-spec transform(_Args::#args{}) -> {ok, syntaxTree()} | {error, string()}.
transform(_Args=#args{current_file_name = File, 
		      focus_sel = Expr,
		      user_inputs = [Pos]}) ->
    ?FULL_TD_TP([remove(Expr, Pos)], [File]).

%%------------------------------------------------------------------------------
%% Rules 

remove(Expr, RawPos) ->
    ?RULE(
       ?T("[Tuples@@@]"),
       begin
	   {Pos, _} = string:to_integer(RawPos),
	   ?TO_AST("[" ++ ?PP(removeElement(Tuples@@@, Pos-1, [])) ++ "]")
       end,
       begin
	   case checkValidSkeletons(Tuples@@@) of
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

-spec removeElement([syntaxTree()], integer(), [syntaxTree()]) -> [syntaxTree()].
removeElement([], 0, Acc) ->
    lists:reverse(Acc);
removeElement([_Removed | Rest], 0, Acc) ->
    lists:flatten([lists:reverse(Acc) | Rest]);
removeElement([Head | Rest], Pos, Acc) ->
    removeElement(Rest, Pos-1, [Head | Acc]).
