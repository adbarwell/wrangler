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
	    io:format("multiplePipe(Tuples, File)~n")
	    %% multiplePipe(Tuples, File)
    end.

singlePipe([Tuple], File) ->
    Kind = skeletonKind(Tuple),
    ?print(Kind),
    case Kind of
	seq ->
	    Type = typeCheckSkeleton(File, Tuple, Kind, both);
	pipe ->
	    InputType = typeCheckSkeleton(File, Tuple, Kind, first),
	    OutputType = typeCheckSkeleton(File, Tuple, Kind, last),
	    Type = {InputType, OutputType};
	farm ->
	    Type = typeCheckSkeleton(File, Tuple, Kind, both);
	ord ->
	    Type = typeCheckSkeleton(File, Tuple, Kind, both);
	map ->
	    Type = typeCheckSkeleton(File, Tuple, Kind, both);
	cluster ->
	    Type = typeCheckSkeleton(File, Tuple, Kind, both);
	reduce ->
	    Type = typeCheckSkeleton(File, Tuple, Kind, both);
	feedback ->
	    Type = typeCheckSkeleton(File, Tuple, Kind, both)
    end,
    ?print(Type).
    %% typeCheckSkeleton(skeletonKind(Tuple), File).

typeCheckSkeleton(File, Tuple, Kind, both) ->
    {typeCheckSkeleton(File, Tuple, Kind, first), typeCheckSkeleton(File, Tuple, Kind, last)};
typeCheckSkeleton(File, Tuple, seq, first) ->
    SeqFun = unwrapSeq(Tuple),
    getFunType(File, SeqFun, arg);
typeCheckSkeleton(File, Tuple, seq, last) ->
    SeqFun = unwrapSeq(Tuple),
    getFunType(File, SeqFun, ret);
typeCheckSkeleton(File, Tuple, pipe, first) ->
    FirstElementFun = unwrapPipe(Tuple, first),
    getFunType(File, FirstElementFun, arg);
typeCheckSkeleton(File, Tuple, pipe, last) ->
    LastElementFun = unwrapPipe(Tuple, last),
    getFunType(File, LastElementFun, ret);
typeCheckSkeleton(File, Tuple, farm, first) ->
    FirstElementFun = unwrapFarm(Tuple, first),
    getFunType(File, FirstElementFun, arg);
typeCheckSkeleton(File, Tuple, farm, last) ->
    LastElementFun = unwrapFarm(Tuple, last),
    getFunType(File, LastElementFun, ret);
typeCheckSkeleton(File, Tuple, ord, first) ->
    FirstElementFun = unwrapOrd(Tuple, first),
    getFunType(File, FirstElementFun, arg);
typeCheckSkeleton(File, Tuple, ord, last) ->
    LastElementFun = unwrapFarm(Tuple, last),
    getFunType(File, LastElementFun, ret);
typeCheckSkeleton(File, Tuple, map, first) ->
    FirstElementFun = unwrapMap(Tuple, first),
    getFunType(File, FirstElementFun, arg);
typeCheckSkeleton(File, Tuple, map, last) ->
    LastElementFun = unwrapMap(Tuple, last),
    getFunType(File, LastElementFun, arg);
typeCheckSkeleton(File, Tuple, cluster, first) ->
    FirstElementFun = unwrapCluster(Tuple, first),
    getFunType(File, FirstElementFun, arg);
typeCheckSkeleton(File, Tuple, cluster, last) ->
    LastElementFun = unwrapCluster(Tuple, first),
    getFunType(File, LastElementFun, ret);
typeCheckSkeleton(File, Tuple, reduce, first) ->
    RedFun = unwrapReduce(Tuple, first),
    getFunType(File, RedFun, arg);
typeCheckSkeleton(File, Tuple, reduce, last) ->
    RedFun = unwrapReduce(Tuple, last),
    getFunType(File, RedFun, ret);
typeCheckSkeleton(File, Tuple, feedback, first) ->
    FirstElementFun = unwrapFeedback(Tuple, first),
    getFunType(File, FirstElementFun, arg);
typeCheckSkeleton(File, Tuple, feedback, last) ->
    LastElementFun = unwrapFeedback(Tuple, last),
    getFunType(File, LastElementFun, ret).


unwrapSeq({tree, tuple, _, X}) ->
    unwrapSeq(X);
unwrapSeq([_, {tree, implicit_fun, Args, _}]) ->
    getFunDef(Args#attr.ann);
unwrapSeq(X) ->
    ?print(X).

unwrapPipe({tree, tuple, _, Xs}, Y) ->
    unwrapPipe(Xs, Y);
unwrapPipe([{wrapper, atom, _, _}, {tree, list, _, Xs} | _], first) ->
    ?print(tuple_size(Xs)),
    FirstElement = element(2, Xs),
    ?print(?PP(FirstElement)),
    unwrapUnknownElement(FirstElement, first);
unwrapPipe(X, first) ->
    ?print(X);
unwrapPipe([{wrapper, atom, _, _}, {tree, list, _, Xs} | _], last) ->
    unwrapPipe(Xs, last);
unwrapPipe({list, A1, none}, last) ->
    io:format("{list, A1, none}~n"),
    unwrapUnknownElement(A1, last);
unwrapPipe({list, _, A2}, last) ->
    io:format("{list, _, A2}~n"),
    unwrapPipe(A2, last);
unwrapPipe({tree, list, _, Xs}, last) ->
    unwrapPipe(Xs, last);
unwrapPipe(X, last) ->
    ?print(X).

unwrapFarm({tree, tuple, _, Xs}, Y) ->
    unwrapFarm(Xs, Y);
unwrapFarm([{wrapper, atom, _, _}, {tree, list, _, Xs} | _], first) ->
    FirstElement = element(2, Xs),
    unwrapUnknownElement(FirstElement, first);
unwrapFarm(X, first) ->
    ?print(X);
unwrapFarm([{wrapper, atom, _, _}, {tree, list, _, Xs} | _], last) ->
    unwrapFarm(Xs, last);
unwrapFarm({list, A1, none}, last) ->
    io:format("{list, A1, none}~n"),
    unwrapUnknownElement(A1, last);
unwrapFarm({list, _, A2}, last) ->
    io:format("{list, _, A2}~n"),
    unwrapFarm(A2, last);
unwrapFarm({tree, list, _, Xs}, last) ->
    unwrapFarm(Xs, last);
unwrapFarm(X, last) ->
    ?print(X).

unwrapOrd(Tuple, X) ->
    unwrapPipe(Tuple, X).

unwrapMap(Tuple, X) ->
    unwrapFarm(Tuple, X).

unwrapCluster(Tuple, X) ->
    unwrapFarm(Tuple, X).

unwrapReduce({tree, tuple, _, Xs}, Y) ->
    unwrapReduce(Xs, Y);
unwrapReduce([{wrapper, atom, _, _}, {tree, implicit_fun, Args, _} | _], _) ->
    getFunDef(Args#attr.ann);
unwrapReduce(X, _) ->
    ?print(X).

unwrapFeedback(Tuple, X) ->
    unwrapFarm(Tuple, X).

unwrapUnknownElement([Element], X) ->
    Kind = skeletonKind(Element),
    case Kind of
	seq ->
	    unwrapSeq(Element);
	_else ->
	    ?print(Kind)
    end;
unwrapUnknownElement(Element, X) ->
    ?print(Element),
    Kind = skeletonKind(Element),
    case Kind of 
	seq ->
	    unwrapSeq(Element);
	_else ->
	    ?print(Kind)
    end.

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


	    
%% multiplePipe(Tuples, File) ->
%%     First = hd(Tuples),
%%     Last = lists:last(Tuples),
%%     ?print(?PP(First)),
%%     ?print(?PP(Last)),
%%     InType = getInputType(skeletonKind(First), File),
%%     OutType = getOutputType(skeletonKind(Last), File),
%%     Compatibility = areCompatible(InType, OutType),
%%     ?print(InType),
%%     ?print(OutType),
%%     ?print(Compatibility).

skeletonKind(Tuple) ->
    Ps = [
	  {seq, ?MATCH(?T("{seq, SeqRest@@@}"), Tuple)},
	  {pipe, ?MATCH(?T("{pipe, PipeRest@@@}"), Tuple)},
	  {farm, ?MATCH(?T("{farm, FarmRest@@@}"), Tuple)},
	  {ord, ?MATCH(?T("{ord, OrdRest@@@}"), Tuple)},
	  {reduce, ?MATCH(?T("{reduce, RedRest@@@}"), Tuple)},
	  {map, ?MATCH(?T("{map, MapRest@@@}"), Tuple)},
	  {cluster, ?MATCH(?T("{cluster, ClustRest@@@}"), Tuple)},
	  {feedback, ?MATCH(?T("{feedback, FBackRest@@@}"), Tuple)}
	 ],
   determineKind(Ps).

determineKind([{_, false}]) ->
    io:format("We've a problem.~n");
determineKind([{Kind, true}]) ->
    Kind;
determineKind([{Kind, true} | _Rest]) ->
    Kind;
determineKind([_ | Rest]) ->
    determineKind(Rest).

%% getInputType({_ , Tuple}, File) ->
%%     ?print(?PP(Tuple)),
%%     {M, F, A} = traverseTuple(Tuple, first),
%%     io:format("M:F/A: ~p:~p/~p~n", [M, F, A]),
%%     {ok, CM} = api_refac:module_name(File),
%%     case CM == M of
%% 	true ->
%% 	    case getFileTypes(F, A, ntyper:rshow(File)) of
%% 		{F, A, {{args, [Arg]}, {ret, Ret}}} ->
%% 		    ?print(Arg),
%% 		    ?print(Ret),
%% 		    Arg;
%% 		{error, Msg} ->
%% 		    io:format("error~n"),
%% 		    ?print(Msg);
%% 		X ->
%% 		    ?print(X)
%% 	    end;
%% 	false ->
%% 	    io:format("False~n")
%%     end.

%% getOutputType({_, Tuple}, File) ->
%%     ?print(?PP(Tuple)),
%%     {M, F, A} = traverseTuple(Tuple, last),
%%     io:format("M:F/A: ~p:~p/~p~n", [M, F, A]),
%%     {ok, CM} = api_refac:module_name(File),
%%     case CM == M of
%% 	true ->
%% 	    case getFileTypes(F, A, ntyper:rshow(File)) of
%% 		{F, A, {{args, [Arg]}, {ret, Ret}}} ->
%% 		    ?print(Arg),
%% 		    ?print(Ret),
%% 		    Ret;
%% 		{error, Msg} ->
%% 		    io:format("error~n"),
%% 		    ?print(Msg);
%% 		X ->
%% 		    ?print(X)
%% 	    end;
%% 	false ->
%% 	    io:format("False~n")
%%     end.

%% areCompatible(any, _) ->
%%     true;
%% areCompatible(_, any) ->
%%     true;
%% areCompatible({c, number, _, _}, {c, number, _, _}) ->
%%     true;
%% areCompatible(X, Y) ->
%%     X == Y.

%% typeCheckSkeleton({reduce, Tuple}, File) ->
%%     ?print(?PP(Tuple)),
%%     reduce;
%%     %% ?print(Tuple),
%%     %% {M, F, A} = traverseReduceTuple(Tuple),
%%     %% io:format("M:F/A: ~p:~p/~p~n", [M, F, A]),
%%     %% {ok, CM} = api_refac:module_name(File),
%%     %% case CM == M of
%%     %% 	true ->
%%     %% 	    case getFileTypes(F, A, ntyper:rshow(File)) of
%%     %% 		{F, A, {{args, [Arg]}, {ret, Ret}}} ->
%%     %% 		    ?print(Arg),
%%     %% 		    ?print(Ret),
%%     %% 		    ?print(Arg == Ret);
%%     %% 		{error, Msg} ->
%%     %% 		    ?print(Msg)
%%     %% 	    end,
%%     %% 	    io:format("True~n");
%%     %% 	false ->
%%     %% 	    io:format("False~n")
%%     %% end;
%% typeCheckSkeleton({feedback, Tuple}, File) ->
%%     ?print(?PP(Tuple)),
%%     feedback;
%% typeCheckSkeleton({_, Tuple}, File) ->
%%     ?print(?PP(Tuple)),
%%     ?print(Tuple),
%%     {M, F, A} = traverseTuple(Tuple, first),
%%     io:format("M:F/A: ~p:~p/~p~n", [M, F, A]),
%%     {ok, CM} = api_refac:module_name(File),
%%     case CM == M of
%% 	true ->
%% 	    case getFileTypes(F, A, ntyper:rshow(File)) of
%% 		{F, A, {{args, [Arg]}, {ret, Ret}}} ->
%% 		    ?print(Arg),
%% 		    ?print(Ret),
%% 		    ?print(Arg == Ret);
%% 		{error, Msg} ->
%% 		    ?print(Msg)
%% 	    end,
%% 	    io:format("True~n");
%% 	false ->
%% 	    io:format("False~n")
%%     end.

%% traverseReduceTuple({tree, tuple, _, A3}) ->
%%     ?print(A3),
%%     traverseReduceTuple(A3);
%% traverseReduceTuple([{wrapper, atom, _, _} | Rest]) ->
%%     traverseReduceTuple(Rest);
%% traverseReduceTuple(X) ->
%%     ?print(X).

%% traverseTuple(Tuple, first) ->
%%     io:format("first~n"),
%%     findFirst(Tuple);
%% traverseTuple(Tuple, last) ->
%%     io:format("last~n"),
%%     findLast(Tuple).

%% findFirst({tree, tuple, _A2, A3}) ->
%%     io:format("tree, tuple~n"),
%%     findFirst(A3);
%% findFirst({tree, implicit_fun, A2, A3}) ->
%%     io:format("tree, implicit_fun~n"),
%%     FunDef = getFunDef(A2#attr.ann),
%%     %% ?print(FunDef),
%%     FunDef;
%% findFirst({tree, list, A2, A3}) ->
%%     io:format("tree, list~n"),
%%     findFirst(A3);
%% findFirst({wrapper, A1, A2, A3}) ->
%%     io:format("wrapper~n"),
%%     ?print(A3),
%%     none;
%% findFirst([Tuple]) ->
%%     io:format("[Tuple]~n"),
%%     findFirst(Tuple);
%% findFirst([{wrapper, atom, A2, A3} | Rest]) ->
%%     io:format("[wrapper, atom | Rest]~n"),
%%     ?print(A3),
%%     ?print(Rest),
%%     findFirst(Rest);
%% findFirst([{tree, list, A2, A3} | Rest]) ->
%%     io:format("[tree, list | Rest]~n"),
%%     B1 = findFirst(A3),
%%     B2 = findFirst(Rest),
%%     ?print(B1),
%%     ?print(B2),
%%     case B1 == none of
%% 	true ->
%% 	    B2;
%% 	false ->
%% 	    B1
%%     end;
%% findFirst([{tree, implicit_fun, A2, A3} | _]) ->
%%     io:format("[tree, implicit_fun | _]~n"),
%%     ?print(A2),
%%     ?print(A3),
%%     FunDef = getFunDef(A2#attr.ann),
%%     ?print(FunDef),
%%     FunDef;
%% findFirst({list, Xs, _}) ->
%%     io:format("{list, Xs, _}~n"),
%%     ?print(Xs),
%%     findFirst(Xs);
%% findFirst(X) ->
%%     ?print(X).

%% findLast({tree, tuple, _A2, A3}) ->
%%     io:format("tree, tuple~n"),
%%     findLast(A3);
%% findLast({tree, implicit_fun, A2, A3}) ->
%%     io:format("tree, implicit_fun~n"),
%%     FunDef = getFunDef(A2#attr.ann),
%%     %% ?print(FunDef),
%%     FunDef;
%% findLast({tree, list, A2, A3}) ->
%%     io:format("tree, list~n"),
%%     findLast(A3);
%% findLast({wrapper, A1, A2, A3}) ->
%%     io:format("wrapper~n"),
%%     ?print(A3),
%%     none;
%% findLast([Tuple]) ->
%%     io:format("[Tuple]~n"),
%%     findLast(Tuple);
%% findLast([{wrapper, atom, A2, A3} | Rest]) ->
%%     io:format("[wrapper, atom | Rest]~n"),
%%     ?print(A3),
%%     ?print(Rest),
%%     findLast(Rest);
%% findLast([{tree, list, A2, A3} | Rest]) ->
%%     io:format("[tree, list | Rest]~n"),
%%     B1 = findLast(A3),
%%     B2 = findLast(Rest),
%%     ?print(B1),
%%     ?print(B2),
%%     case B1 == none of
%% 	true ->
%% 	    B2;
%% 	false ->
%% 	    B1
%%     end;
%% findLast([{tree, implicit_fun, A2, A3} | _]) ->
%%     io:format("[tree, implicit_fun | _]~n"),
%%     ?print(A2),
%%     ?print(A3),
%%     FunDef = getFunDef(A2#attr.ann),
%%     ?print(FunDef),
%%     FunDef;
%% findLast({list, Xs, _}) ->
%%     io:format("{list, Xs, _}~n"),
%%     ?print(Xs),
%%     findLast(Xs);
%% findLast(X) ->
%%     ?print(X).

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
