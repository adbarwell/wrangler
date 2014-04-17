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
    {InputType, OutputType} = typeCheckSkeleton(File, Tuple, both),
    ?print(InputType),
    ?print(OutputType).

multiplePipe(Tuples, File) ->
    First = hd(Tuples),
    InputType = typeCheckSkeleton(File, First, first),
    Last = lists:last(Tuples),
    OutputType = typeCheckSkeleton(File, Last, last),
    ?print(InputType),
    ?print(OutputType).

typeCheckSkeleton(File, Tuple, both) ->
    {typeCheckSkeleton(File, Tuple, first), 
     typeCheckSkeleton(File, Tuple, last)};
typeCheckSkeleton(File, Tuple, first) ->
    getFunType(File, unwrapTuple(Tuple, first), arg);
typeCheckSkeleton(File, Tuple, last) ->
    getFunType(File, unwrapTuple(Tuple, last), ret).

unwrapTuple({tree, tuple, _, X}, FirstOrLast) ->
    unwrapTuple(X, FirstOrLast);
unwrapTuple([_, {tree, implicit_fun, Args, _} | _], _) ->
    getFunDef(Args#attr.ann);
unwrapTuple([_, {tree, list, _, Xs} | _], first) ->
    [T] = element(2, Xs), 
    unwrapTuple(T, first);
unwrapTuple([_, {tree, list, _, Xs} | _], last) ->
    unwrapTuple(Xs, last);
unwrapTuple({list, [A1], none}, last) ->
    unwrapTuple(A1, last);
unwrapTuple({list, _, A2}, last) ->
    unwrapTuple(A2, last);
unwrapTuple({tree, list, _, Xs}, last) ->
    unwrapTuple(Xs, last);
unwrapTuple(X, _) ->
    {error, "unwrapTuple encountered unknown form"}.

getFunType(File, {M, F, A}, arg) ->
    {ok, CM} = api_refac:module_name(File),
    case CM == M of
	true ->
	    case getFileTypes(F, A, ntyper:rshow(File)) of
		{F, A, {{args, Args}, {ret, Ret}}} ->
		    Args;
		{error, Msg} ->
		    io:format("error: Msg: ~p~n", [Msg]),
		    error
	    end;
	false ->
	    io:format("Not current Module~n"),
	    ncm
    end;
getFunType(File, {M, F, A}, ret) ->
    {ok, CM} = api_refac:module_name(File),
    case CM == M of
	true ->
	    case getFileTypes(F, A, ntyper:rshow(File)) of
		{F, A, {{args, _Args}, {ret, Ret}}} ->
		    Ret;
		{error, Msg} ->
		    io:format("error Msg: ~p~n", [Msg]),
		    error
	    end;
	false ->
	    io:format("Not current Module~n"),
	    ncm
    end.

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
	?MATCH(?T("{pipe, PipeRest@@@}"), Tuple) or
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
