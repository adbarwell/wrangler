-module(refac_primitive_list_to_binary).

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
    [].

-spec select_focus(Args::#args{}) -> {ok, syntaxTree()} | {ok, none}.
select_focus(_Args=#args{current_file_name = File,
			 highlight_range = {Start, End}}) ->
    api_interface:pos_to_expr1(File, Start, End).

-spec check_pre_cond(Args::#args{}) -> ok.
check_pre_cond(_) ->
    ok.

-spec selective() -> boolean().
selective() ->
    true.

-spec transform(Args::#args{}) -> {ok, syntaxTree()} | {error, string()}.
transform(_Args=#args{current_file_name=File, focus_sel=Expr}) ->
    %% Start1 = api_refac:start_end_loc(Expr),
    %% io:format("Start1: ~p~n", [Start1]),
    ?FULL_TD_TP([std_list(File, Expr), skel(File, Expr)], [File]).

%%------------------------------------------------------------------------------
%% Rules 

std_list(File, Expr) ->
    ?print(std_list),
    ?RULE(?T("N@ = Lst@@"),
	  begin
	      ?TO_AST("N@ = " ++ convertList(Lst@@))
	  end,
	  begin
	      case locationCheck(Expr, _This@) of
		  true ->
		      %% typing(File, Expr, Lst@@, N@),
		      ?print(convertList(Lst@@)),
		      checkRange(Lst@@);
		  _ ->
		      false
	      end
	  end).

skel(File, Expr) ->
    ?print(skel),
    ?RULE(?T("skel:do(Pipe@, In@)"),
	  begin
	      ?TO_AST("skel:do([{seq, fun ?MODULE:fib/1}], In@)")
	  end,
	  begin
	      case locationCheck(Expr, _This@) of
		  true ->
		      ?print(adjustPipe(File, Pipe@)),
		      true;
		  _ ->
		      false
	      end
	  end).

%%------------------------------------------------------------------------------
%% Assisting Functions 

-spec locationCheck(syntaxTree(), syntaxTree()) -> boolean().
locationCheck(Expr, _This@) ->
    api_refac:start_end_loc(_This@) == api_refac:start_end_loc(Expr).

typing(File, Expr, Lst@@, N@) ->
    Ftypes = ntyper:rshow(File),
    ?print(Ftypes).

checkRange(T) ->
    lists:foldl(fun(X, Acc) ->
			(X >= 0) and (X =< 255)
		end,
		true,
		extractList(T)).

extractList([LstTuple]) ->
    extractList_1(LstTuple, []);
extractList(LstTuple) ->
    extractList_1(LstTuple, []).

extractList_1({tree, list, _, A4}, Acc) ->
    extractList_1(A4, Acc);
extractList_1({list, A2, none}, Acc) ->
    case resolveLstElement(A2, Acc) of
	{error, X} ->
	    ?print(X);
	Acc2 ->
	    lists:reverse(Acc2)
    end;
extractList_1({list, A2, A3}, Acc) ->
    case resolveLstElement(A2, Acc) of
	{error, X} ->
	    ?print(X);
	Acc2 ->
	    extractList_1(A3, Acc2)
    end;
extractList_1(X, Acc) ->
    ?print(X),
    ?print(Acc).

resolveLstElement(A2, Acc) ->
    case A2 of
	[{wrapper, integer, _, {integer, _, A4}}] ->
	    [list_to_integer(A4) | Acc];
	_ ->
	    ?print(A2),
	    {error, "A2 is not an integer"}
    end.

convertList(T) ->
    Lst = extractList(T),
    BinStr = getListContentStr(Lst),
    ?print(Lst),
    ?print(BinStr),
    BinStr.

getListContentStr(Lst) ->
    "<<" ++ getListContentStr_1(Lst) ++ ">>".

getListContentStr_1([H]) ->
    hd(io_lib:format("~w", [H]));
getListContentStr_1([H | Rest]) ->
    hd(io_lib:format("~w", [H])) ++ "," ++  getListContentStr_1(Rest).

adjustPipe(File, Pipe) ->
    ?print(Pipe),
    adjustFuns(File, retrieveFunctions(Pipe, [])).

retrieveFunctions({tree, list, _, A4}, Funs) ->
    ?print(1),
    retrieveFunctions(A4, Funs);
retrieveFunctions({list, A2, none}, Funs) ->
    ?print(2),
    retrieveFunctions(A2, Funs);
retrieveFunctions({list, A2, A3}, Funs) ->
    ?print(3),
    retrieveFunctions(A3, retrieveFunctions(A2, Funs));
retrieveFunctions([], Funs) ->
    ?print(4),
    Funs;
retrieveFunctions([{tree, tuple, _, A4} | Rest], Funs) ->
    ?print(5),
    retrieveFunctions(Rest, retrieveFunctions(A4, Funs));
retrieveFunctions([{wrapper, atom, _, _}, {tree, implicit_fun, Args, _} | _], Funs) ->
    ?print(6),
    [getFunDef(Args#attr.ann) | Funs];
retrieveFunctions([{wrapper, atom, _, _}, A2 | _], Funs) ->
    ?print(7),
    retrieveFunctions(A2, Funs);
retrieveFunctions(X, Funs) ->
    ?print(Funs),
    ?print(X),
    [{error, "Unexpected Form"} | Funs]. 

getFunDef([{fun_def, {M, F, A, _, _}} | _]) ->
    {M, F, A};
getFunDef([_ | Rest]) ->
    getFunDef(Rest).

adjustFuns(File, [Hd]) ->
    adjustFun(File, Hd);
adjustFuns(File, [Hd | Rest]) ->
    adjustFun(File, Hd),
    adjustFuns(File, Rest).

adjustFun(File, MFA) ->
    FunDef = api_refac:mfa_to_fun_def(File, MFA),
    FunStr = ?PP(FunDef),
    PropAST = ?TO_AST("Y = binary_to_list(X)"),
    ?print(FunStr),
    ?print(FunDef),
    ?print(PropAST).
