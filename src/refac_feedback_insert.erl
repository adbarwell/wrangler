-module(refac_feedback_insert).
-behaviour(gen_refac).

-include("../include/wrangler.hrl").
%% -include("../include/wrangler_internal.hrl").

-export([input_par_prompts/0, 
	 select_focus/1, 
	 check_pre_cond/1, 
	 selective/0, 
	 transform/1]).

-compile([export_all]).

-ifndef(debug).
-define(debug, true).
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
     "Name of Module containing Constraint function: ",
     "Name of Constraint Function: "
    ].

-spec select_focus(Args::#args{}) -> {ok, syntaxTree()} | {ok, none}.
select_focus(_Args=#args{current_file_name = File,
			 highlight_range = {Start, End}}) ->
    api_interface:pos_to_expr1(File, Start, End).

-spec check_pre_cond(Args::#args{}) -> ok.
check_pre_cond(_Args=#args{current_file_name = File,
			   user_inputs = [Mod, Fun]}) ->
    checkConstraintFun(File, Mod, Fun).

-spec selective() -> boolean().
selective() ->
    true.

-spec transform(_Args::#args{}) -> {ok, syntaxTree()} | {error, string()}.
transform(_Args=#args{current_file_name = File,
		      focus_sel = Expr,
		      user_inputs = [Mod, Fun]
		     }) ->
    %% io:format("Args: ~p~n", [Args]),
    ?FULL_TD_TP([
		 workflow(File, Expr, Mod, Fun)
		 %% workflow(Expr), 
		 %% seq(Expr), 
		 %% farm(Expr), 
		 %% map(Expr), 
		 %% mapWorkers(Expr),
		 %% cluster(Expr),
		 %% reduce(Expr),
		 %% feedback(Expr)
		], [File]).

%%------------------------------------------------------------------------------
%% Rules

%%------------------------------------------------------------------------------
%% Work in progress

workflow(File, Expr, Mod, Fun) ->
    ?RULE(
       ?T("[Tuples@@@]"),
       begin
	   ModStr = replaceModIfCurrent(File, Mod),
	   ?TO_AST("[{feedback, [" ++ ?PP(Tuples@@@) ++ "], fun " ++ 
		       ModStr ++ ":" ++ Fun ++ "/1}]")
       end,
       begin
	   Matched = checkValidSkeletons(Tuples@@@),
	   case Matched of
	       true ->
		   case locationCheck(Expr, _This@) of
		       true ->
			   playing(Tuples@@@, File),
			   true;
		       false ->
			   false
		   end;
	       false ->
		   false
	   end
       end).

playing(Tuples, File) ->
    case length(Tuples) of
	1 ->
	    singlePipe(Tuples, File);
	_ ->
	    multiplePipe(Tuples, File)
    end.

singlePipe([Tuple], File) ->
    typeCheckSkeleton(skeletonKind(Tuple), File).

multiplePipe(Tuples, File) ->
    First = hd(Tuples),
    Last = lists:last(Tuples),
    ?print(?PP(First)),
    ?print(?PP(Last)),
    InType = getInputType(skeletonKind(First), File),
    OutType = getOutputType(skeletonKind(Last), File),
    Compatibility = areCompatible(InType, OutType),
    ?print(InType),
    ?print(OutType),
    ?print(Compatibility).

skeletonKind(Tuple) ->
    Ps = [
	  {seq, ?MATCH(?T("{seq, SeqRest@@@}"), Tuple)},
	  {farm, ?MATCH(?T("{farm, FarmRest@@@}"), Tuple)},
	  {ord, ?MATCH(?T("{ord, OrdRest@@@}"), Tuple)},
	  {reduce, ?MATCH(?T("{reduce, RedRest@@@}"), Tuple)},
	  {map, ?MATCH(?T("{map, MapRest@@@}"), Tuple)},
	  {cluster, ?MATCH(?T("{cluster, ClustRest@@@}"), Tuple)},
	  {feedback, ?MATCH(?T("{feedback, FBackRest@@@}"), Tuple)}
	 ],
   determineKind(Ps, Tuple).

determineKind([{_, false}], _Tuple) ->
    io:format("We've a problem.~n");
determineKind([{Kind, true}], Tuple) ->
    {Kind, Tuple};
determineKind([{Kind, true} | _Rest], Tuple) ->
    {Kind, Tuple};
determineKind([_ | Rest], Tuple) ->
    determineKind(Rest, Tuple).

getInputType({seq, Tuple}, File) ->
    ?print(?PP(Tuple)),
    {M, F, A} = traverseTuple(Tuple),
    io:format("M:F/A: ~p:~p/~p~n", [M, F, A]),
    {ok, CM} = api_refac:module_name(File),
    case CM == M of
	true ->
	    case getFileTypes(F, A, ntyper:rshow(File)) of
		{F, A, {{args, [Arg]}, {ret, Ret}}} ->
		    ?print(Arg),
		    ?print(Ret),
		    Arg;
		{error, Msg} ->
		    ?print(Msg)
	    end;
	false ->
	    io:format("False~n")
    end.

getOutputType({seq, Tuple}, File) ->
    ?print(?PP(Tuple)),
    {M, F, A} = traverseTuple(Tuple),
    io:format("M:F/A: ~p:~p/~p~n", [M, F, A]),
    {ok, CM} = api_refac:module_name(File),
    case CM == M of
	true ->
	    case getFileTypes(F, A, ntyper:rshow(File)) of
		{F, A, {{args, [Arg]}, {ret, Ret}}} ->
		    ?print(Arg),
		    ?print(Ret),
		    Ret;
		{error, Msg} ->
		    ?print(Msg)
	    end;
	false ->
	    io:format("False~n")
    end.

areCompatible(any, _) ->
    true;
areCompatible(_, any) ->
    true;
areCompatible(X, Y) ->
    X == Y.

typeCheckSkeleton({seq, Tuple}, File) ->
    ?print(?PP(Tuple)),
    {M, F, A} = traverseTuple(Tuple),
    {ok, CM} = api_refac:module_name(File),
    case CM == M of
	true ->
	    case getFileTypes(F, A, ntyper:rshow(File)) of
		{F, A, {{args, [Arg]}, {ret, Ret}}} ->
		    ?print(Arg),
		    ?print(Ret),
		    ?print(Arg == Ret);
		{error, Msg} ->
		    ?print(Msg)
	    end,
	    io:format("True~n");
	false ->
	    io:format("False~n")
    end.

traverseTuple({tree, tuple, _A2, A3}) ->
    %% ?print(A3),
    traverseTuple(A3);
traverseTuple({tree, implicit_fun, A2, A3}) ->
    FunDef = getFunDef(A2#attr.ann),
    %% ?print(FunDef),
    FunDef;
    %% ?print(A3);
traverseTuple({wrapper, A1, A2, A3}) ->
    ok;
    %% ?print(A1),
    %% ?print(A2),
    %% ?print(A3);
traverseTuple([Tuple]) ->
    %% ?print(Tuple),
    traverseTuple(Tuple);
traverseTuple([{wrapper, A1, A2, A3} | Rest]) ->
    %% ?print(A1),
    %% ?print(A2),
    %% ?print(A3),
    traverseTuple(Rest);
traverseTuple([{tree, A1, A2, A3} | Rest]) ->
    %% ?print(A1),
    %% ?print(A2),
    %% ?print(A3),
    traverseTuple(Rest);
traverseTuple(X) ->
    ?print(X).

getFunDef([{fun_def, {M, F, A, _, _}} | _]) ->
    {M, F, A};
getFunDef([_ | Rest]) ->
    getFunDef(Rest).

%%------------------------------------------------------------------------------
%% Assisting Functions 

-spec locationCheck(syntaxTree(), syntaxTree()) -> boolean().
%% @doc Limits the refactoring to only the selected instance.
locationCheck(Expr, _This@) ->
    api_refac:start_end_loc(_This@) == api_refac:start_end_loc(Expr).

-spec checkValidSkeletons(list()) -> boolean().
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

-spec checkConstraintFun(string(), string(), string()) -> ok | {error, string()}.
%% @doc Checks the given constraint function for validity where possible.
%%      The constraint function must have an arity of one, and return a boolean.
%%      
%%      NB Currently only checks functions found within the same module.
checkConstraintFun(File, Mod, Fun) ->
    case checkArity(File, Mod, Fun) of
	ok ->
	    checkReturnType(File, list_to_atom(Mod), list_to_atom(Fun), 1);
	{error, Msg} ->
	    {error, Msg}
    end.

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

-spec checkReturnType(string(), atom(), atom(), integer()) -> ok | {error, string()}.
%% @doc Checks whether the return type of the given function is boolean.
checkReturnType(File, Mod, Fun, Arity) ->
    {ok, M} = api_refac:module_name(File),
    case M == Mod of
	true ->
	    checkType(File, Fun, Arity);
	false ->
	    io:format("false~n"),
	    ok
    end.

checkType(File, Fun, Arity) ->
    case getFileTypes(Fun, Arity, ntyper:rshow(File)) of
	{Fun, Arity, Sig} ->
	    isBoolean(Sig);
	{error, Msg} ->
	    {error, Msg}
    end.

getFileTypes(F, A, []) ->
    {error, lists:concat(["Could not find function ", F, "/", A,
			  " in given module."])};
getFileTypes(F, A, [{F, A, Sig} | _Rest]) ->
    {F, A, Sig};
getFileTypes(F, A, [_ | Rest]) ->
    getFileTypes(F, A, Rest).

isBoolean({{args, _Args}, {ret, RetTypes}}) ->
    case RetTypes of
	{c, atom, [false], unknown} ->
	    ok;
	{c, atom, [true], unknown} ->
	    ok;
	{c, atom, [false, true], unknown} ->
	    ok;
	_else ->
	    {error, "Given constraint function does not return a boolean value."}
    end.
