-module(refac_make_list_arg_generic).
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
     "Index of list argument: ",
     "Function clause: ",
     "New Argument Name: "
    ].

%% -spec select_focus(_Args::#args{}) -> {ok, syntaxTree()} | {ok, none} | none.
select_focus(_Args=#args{current_file_name = File, 
			 user_inputs = [Index, 
					Clause, Name], cursor_pos = Pos}) ->
    ?print(File),
    ?print(Index),
    ?print(Clause),
    ?print(Name),
    ?print(Pos),
    case api_interface:pos_to_fun_def(File, Pos) of
	{ok, FunDef} ->
	    ?print(FunDef),
	    ?print(?PP(FunDef)),
	    case api_interface:pos_to_fun_name(File, Pos) of
		{ok, {M, F, A, _, _}} ->
		    ?print(M),
		    ?print(F),
		    ?print(A),
		    case ?MATCH(?T("f@(Args@@@) when G@@@ -> C@@@"), FunDef) of
			true ->
			    ?print(true),
			    ?print(Args@@@),
			    ?print(length(Args@@@)),
			    Args = lists:nth(list_to_integer(Clause), Args@@@),
			    ?print(?PP(Args)),
			    Arg = lists:nth(list_to_integer(Index), Args),
			    ?print(?PP(Arg)),
			    {ok, {{M, F, A}, FunDef, Arg}};
			    %% {error, "Halt here, please."};
			false ->
			    ?print(false),
			    %% {ok, {{M, F, A}, FunDef}}
			    {error, "Halt here, please."}
		    end;
		{error, Msg} ->
		    ?print(1),
		    {error, Msg}
	    end;
	{error, Msg} ->
	    ?print(0),
	    {error, Msg}
    end.

-spec check_pre_cond(Args::#args{}) -> ok.
%% check_pre_cond(_Args=#args{current_file_name = File,
%% 			   user_inputs = [Index, Clause], focus_sel = Expr}) ->
check_pre_cond(_) ->
    %% ?print(check_pre_cond),
    %% {{M, F, A}, _FunDef, Arg} = Expr,
    %% ?print(M),
    %% ?print(F),
    %% ?print(A),
    %% ?print(Arg),
    ok.

-spec selective() -> boolean().
selective() ->
    true.

-spec transform(_Args::#args{}) -> {ok, syntaxTree()} | {error, string()}.
transform(Args=#args{current_file_name = F,
		     user_inputs = [I, C, N], focus_sel = S}) ->
    ?FULL_TD_TP([first(F, list_to_integer(I), list_to_integer(C), N, S)], [F]).


%%------------------------------------------------------------------------------
%% Rules

first(File, ArgIndex, ClauseIndex, ArgName, {{M, F, A}, FunDef, Arg}) ->
    ?print(first),
    ?RULE(?T("f@(Args@@@) when Guards@@@ -> Clauses@@@"),
	  begin
	      {Args, Guards, Clause} = {lists:nth(ClauseIndex, Args@@@), 
					lists:nth(ClauseIndex, Guards@@@), 
					lists:nth(ClauseIndex, Clauses@@@)},
	      
	      ?TO_AST(lists:concat([original(Args@@@, Guards@@@, 
						 Clauses@@@, ""), "f@(", 
				    innerArgs(Args, Arg, ArgName), ")", 
				    printGuards(Guards), " -> ", 
				    F, "(", innerArgs(Args, Arg, ArgName), 
				    ");"]))

	  end,
	  begin
	      functionCheck(f@, length(hd(Args@@@)), {M, F, A})
	  end).

%%------------------------------------------------------------------------------
%% Assisting Functions 

functionCheck({tree, atom, _, F}, A, {_, F, A}) ->
    true;
functionCheck(_, _, _) ->
    false.

original([], [], [], Acc) ->
    ?print(Acc),
    Acc;
original([Args | ArgsRest], [Guards | GuardsRest], [Clauses | ClausesRest], Acc) ->
    ?print(?PP(Args)),
    ?print(Guards),
    ?print(?PP(Clauses)),
    case Guards of
	[] ->
	    original(ArgsRest,
		     GuardsRest,
		     ClausesRest,
		     Acc ++ "f@(" ++ ?PP(Args) ++ ") -> " ++ 
			 ?PP(Clauses) ++ "; ");
	_ ->
	    original(ArgsRest,
		     GuardsRest,
		     ClausesRest,
		     Acc ++ "f@(" ++ ?PP(Args) ++ ") when " ++ 
			 printGuards(Guards) ++ " -> " ++ ?PP(Clauses) ++ "; ")
    end.

printGuards([]) ->
    "";
printGuards(Guards) ->
    printGuards(Guards, "").

printGuards([], Acc) ->
    tl(Acc);
printGuards([Guard | Rest], Acc) ->
    printGuards(Rest, Acc ++ ", " ++ ?PP(Guard)).

innerArgs(Args, Arg, Name) ->
    innerArgs(Args, Arg, Name, "").

%% innerArgs([], _arg, _name, "") ->
%%     ?print(0),
%%     "";
innerArgs([], _arg, _name, Acc) ->
    ?print(1),
    ?print(Acc),
    tl(Acc);
innerArgs([Arg | Rest], Arg, Name, Acc) ->
    ?print(2),
    ?print(Acc),
    ?print(lists:concat([Acc, ", ", Name])),
    innerArgs(Rest, Arg, Name,
	      lists:concat([Acc, ", ", Name]));
innerArgs([A | Rest], Arg, Name, Acc) ->
    ?print(3),
    innerArgs(Rest, Arg, Name, lists:concat([Acc, ",", ?PP(A)])).
