-module(refac_reduce_insert).
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
		 sum(Expr)
		 %% foldl(File, Expr, Mod, Fun), 
		 %% foldr(File, Expr, Mod, Fun)
		], [File]).

%%------------------------------------------------------------------------------
%% Rules

sum(Expr) ->
    ?RULE(
       ?T("lists:sum(Input@@)"),
       begin
	   ?TO_AST("hd(skel:do([{reduce, fun(X, Y) -> X + Y end, fun(X) -> X end}], [Input@@]))")
       end,
       begin
	   locationCheck(Expr, _This@)
       end).

%% foldl(File, Expr, Mod, Fun) ->
%%     ?RULE(
%%        ?T("lists:foldl(Fun@@@, Init@, Input@@)"),
%%        begin
%% 	   ?TO_AST("hd(skel:do([{reduce, fun(X, Y) -> X + Y end, fun(X) -> X end}], [Input@@]))")
%%        end,
%%        begin
%% 	   locationCheck(Expr, _This@)
%%        end).

%% foldr(File, Expr, Mod, Fun) ->
%%     ?RULE(
%%        ?T("lists:foldr(Fun@@@, Init@, Input@@)"),
%%        begin
%% 	   ?TO_AST("hd(skel:do([{reduce, fun(X, Y) -> X + Y end, fun(X) -> X end}], [Input@@]))")
%%        end,
%%        begin
%% 	   locationCheck(Expr, _This@)
%%        end).


%%------------------------------------------------------------------------------
%% Assisting Functions

-spec locationCheck(syntaxTree(), syntaxTree()) -> boolean().
%% @doc Limits the refactoring to only the selected instance.
locationCheck(Expr, _This@) ->
    api_refac:start_end_loc(_This@) == api_refac:start_end_loc(Expr).

-spec checkArity(string(), string(), string()) -> ok | {error, string()}.
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
			      ok | {error | string()}.
checkModFunArity(Mod, Fun, []) ->
    {error,
     "Could not find " ++ atom_to_list(Mod) ++ ":" ++ atom_to_list(Fun) ++
	 "/1 in current module; function must take one argument."};
checkModFunArity(Mod, Fun, [{Mod, Fun, 1} | _])  ->
    ok;
checkModFunArity(Mod, Fun, [_ | Rest]) ->
    checkModFunArity(Mod, Fun, Rest).

-spec replaceModIfCurrent(string(), string()) -> string().
replaceModIfCurrent(File, Mod) ->
    {ok, CurrMod} = api_refac:module_name(File),
    case atom_to_list(CurrMod) == Mod of
	true ->
	    "?MODULE";
	false ->
	    Mod
    end.
