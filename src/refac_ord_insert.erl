-module(refac_ord_insert).
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

-spec select_focus(_Args::#args{}) -> {ok, syntaxTree()} | {ok, none}.
select_focus(_Args=#args{current_file_name = File,
			 highlight_range = {Start, End}}) ->
    api_interface:pos_to_expr1(File, Start, End).

-spec check_pre_cond(_Args::#args{}) -> ok.
check_pre_cond(_Args) ->
    ok.

-spec selective() -> boolean().
selective() ->
    true.

-spec transform(_Args::#args{}) -> {ok, syntaxTree()} | {error, string()}.
transform(_Args=#args{current_file_name = File,
		      focus_sel = Expr}) ->
    ?FULL_TD_TP([
		 workflow(Expr), 
		 seq(Expr), 
		 farm(Expr), 
		 map(Expr), 
		 mapWorkers(Expr),
		 cluster(Expr),
		 reduce(Expr),
		 feedback(Expr)
		], [File]).

%%------------------------------------------------------------------------------
%% Rules

workflow(Expr) ->
    ?RULE(
       ?T("[Tuples@@@]"),
       begin
	   ?TO_AST("[{ord, [" ++ ?PP(Tuples@@@) ++ "]}]")
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

seq(Expr) ->
    ?RULE(
       ?T("{seq, F@@@}"),
       begin
	   ?TO_AST("{ord, [{seq, " ++ ?PP(F@@@) ++ "}]}")
       end,
       locationCheck(Expr, _This@)).

farm(Expr) ->
    ?RULE(
       ?T("{farm, Pipe@@, NW@}"),
       begin
	   ?TO_AST("{ord, [{farm, Pipe@@, NW@}]}")
       end,
       locationCheck(Expr, _This@)).

map(Expr) ->
    ?RULE(
       ?T("{map, Pipe@@}"),
       begin
	   ?TO_AST("{ord, [{map, Pipe@@}]}")
       end,
       locationCheck(Expr, _This@)).

mapWorkers(Expr) ->
    ?RULE(
       ?T("{map, Pipe@@, NW@}"),
       begin
	   ?TO_AST("{ord, [{map, Pipe@@, NW@}]}")
       end,
       locationCheck(Expr, _This@)).


cluster(Expr) ->
    ?RULE(
       ?T("{cluster, Pipe@@, Decomp@@@, Recomp@@@}"),
       begin
	   ?TO_AST("{ord, [{cluster, Pipe@@, " ++ ?PP(Decomp@@@) ++ 
		       ", " ++ ?PP(Recomp@@@) ++ "}]}")
       end,
       locationCheck(Expr, _This@)).

reduce(Expr) ->
    ?RULE(
       ?T("{reduce, Pipe@@, Decomp@@@}"),
       begin
	   ?TO_AST("{ord, [{reduce, Pipe@@, " ++ ?PP(Decomp@@@) ++ "}]}")
       end,
       locationCheck(Expr, _This@)).

feedback(Expr) ->
    ?RULE(
       ?T("{feedback, Pipe@@, CondCheck@@@}"),
       begin
	   ?TO_AST("{ord, [{feedback, Pipe@@, " ++ ?PP(CondCheck@@@) ++ "}]}")
       end,
       locationCheck(Expr, _This@)).


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
