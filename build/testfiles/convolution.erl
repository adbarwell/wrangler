%%----------------------------------------------------------------------
%% #0.    BASIC INFORMATION
%%----------------------------------------------------------------------
%% File:       ...
%% Author       : ...
%% Description  : ...
%% 
%% Modules used : ...
%% 
%%----------------------------------------------------------------------

%% @doc Describe the functionality of this module.
%%
-module(convolution).
-compile(export_all).

-include_lib("/Users/adam/git/erlang/erl_img/include/erl_img.hrl").

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
%% Type Definitions 

-type erl_img() :: #erl_image{}.
-type img_format() :: gray4 | gray8 | palette4 | palette8 | 
		      b8g8r8 | r8g8b8 | r8g8b8a8.
-type bin_int() :: 0..255.

%%------------------------------------------------------------------------------
%% Definitions 

-define(width, 400).
-define(height, 400).
-define(maskWidth, 8).
-define(maskHeight, 8).

%%------------------------------------------------------------------------------
%% Manually create an image.
 
-spec createRecord([bin_int()]) -> erl_img().
%% @doc Not used at present(?)
createRecord(R) ->    
    Pixels = createPixels(R, 1),
    ThePixMap = #erl_pixmap{top = 0,
                            left = 0, 
			    width = 64, 
			    height = 64, 
			    palette = undefined, 
			    format = r8g8b8a8, 
			    attributes=[], 
			    pixels  = Pixels},                                  
    Image = #erl_image{type = image_png,name = undefined,
               filename = "blah.png",size = undefined,
               extension = undefined,mtime = undefined,itime = undefined,
               comment = [],format = r8g8b8a8,width = 64,height = 64,
               depth = 8,bytes_pp = 3,alignment = 1,
               attributes = [{'ColorType',6},
                             {'Compression',0},
                             {'Filter',0},
                             {'Interlace',0}],
               order = left_to_right,
               palette = undefined,
               pixmaps = [ThePixMap]},
               
    Image.
    
-spec createPixels([bin_int()], non_neg_integer()) -> [{non_neg_integer(), binary()}].
createPixels([],_) -> 
    [];
createPixels([H|T], N) ->
    [{N, list_to_binary(H)} | createPixels(T, N+1)].

%%------------------------------------------------------------------------------
%% Input handling     

-spec createInput(string()) -> {[[bin_int()]], [byte()]}.
createInput(Filename) ->
    {R, F1} = readImage(Filename),
    Msk = [0 || _ <- lists:seq(1, ?maskWidth * ?maskHeight)],
    Val = 1.0 / (?maskWidth * 2.0 - 1.0),
    
    Y = ?maskHeight/2,

    %% Msk2 = setListElement(Msk, Y*maskWidth(), 0, Val, fun(N) -> (maskHeight()/2) * maskWidth() + N end),
    Msk2 = setListElement1(round(Y), 0, ?maskWidth, Val, Msk),
    ?print(Msk2),
    
    X = ?maskWidth/2,
    %% Msk3 = setListElement(Msk2, 0, 0, Val, fun(N) -> N * maskWidth() + (maskWidth()/2) end),
    Msk3 = setListElement2(round(X), 0, ?maskHeight, Val, Msk2),
    
    {R, Msk3}.

-spec readImage(string()) -> {[[bin_int()]], img_format()}.
readImage(FileName) ->	
      {ok, _Img=#erl_image{format=F1, pixmaps=[PM]}} = erl_img:load(FileName),
      #erl_pixmap{pixels=Rows} =PM,
      R = lists:map(fun({A,B}) -> B end, Rows),
      {R, F1}.

-spec setListElement1(non_neg_integer(), 
		      non_neg_integer(), 
		      non_neg_integer(),
		      float(),
		      [bin_int()]) ->
			     [bin_int()].
setListElement1(_Y, End, End, _Val, List) -> 
    List; 
setListElement1(Y, I, End, Val, List) -> 
    NewList = setElement(Y * ?maskWidth + (I+1), List, Val),
    setListElement1(Y, I+1, End, Val, NewList).
    
-spec setListElement2(non_neg_integer(),
		      non_neg_integer(),
		      non_neg_integer(),
		      float(),
		      [bin_int()]) ->
			     [bin_int()].
setListElement2(_X, End, End, _Val, List) -> 
    List;
setListElement2(X, I, End, Val, List) ->
    % io:fwrite("~p~n", [(I+1)*maskWidth()+X]),
    NewList = setElement((I+1) * ?maskWidth + X, List, Val),
    setListElement2(X, I+1, End, Val, NewList).

%% -spec setElement(pos_integer(), [bin_int()], bin_int()) -> [bin_int()]. 
%% setElement(_I, [], _New) -> 
%%     [];
%% setElement(1, [_|Rest], New) -> 
%%     [New|Rest];
%% setElement(I, [E|Rest], New) -> 
%%     [E | setElement(I-1, Rest, New)].

-spec setElementB(pos_integer(), binary(), bin_int()) -> binary().
setElementB(_I, <<>>, _New) ->
    <<>>;
setElementB(1, <<_, Rest/binary>>, New) ->
    <<New, Rest/binary>>;
setElementB(I, <<E, Rest/binary>>, New) ->
    IB = setElementB(I-1, Rest, New),
    <<E, IB/binary>>.

setElement(_I, [], _New) ->
    [];
setElement(1, [_ | Rest], New) ->
    [New | Rest];
setElement(I, Rest, New) ->
    setElement(I-1, Rest, New).

test([], Etc) ->
    [];
test(Etc, []) ->
    [].

test([]) ->
    [].

%% setElement(1, [_|Rest], New) -> [New|Rest];
%% setElement(I, [E|Rest], New) -> [E|setElement(I-1, Rest, New)].    
   
   
%% setListElement([], A, B, C, D) -> [];
%% setListElement([ L | Ls ], Pos, Pos, Val, Acc) -> [ Val | setListElement(Ls, Acc(Pos), Pos+1, Val, Acc) ];
%% setListElement([ L | Ls ], Pos, N, Val, Acc) -> [ L | setListElement(Ls, Acc(N), N, Val, Acc) ].

%%------------------------------------------------------------------------------
%% Convolution 

-spec convolution([[bin_int()]], [bin_int()]) -> [[bin_int()]].
convolution(Input, Msk) ->
    convolution1(Input, Msk, 0, 0).

-spec convolution1([[bin_int()]], [bin_int()], 
		   non_neg_integer(), non_neg_integer()) -> [[bin_int()]].
convolution1(Input, Msk, X, Y) -> case (X < ?height) of
                                    true -> Input2 = convolution(Input, Msk, X, Y),
                                            convolution1(Input2, Msk, X+1, Y);
                                    false -> Input
                                  end.
%% convolution1(Input, Msk, X, Y) -> Input.

-spec convolution([[bin_int()]], [bin_int()], 
		  non_neg_integer(), non_neg_integer()) -> [[bin_int()]].
convolution(Input, Msk, X, Y)  -> 
   case (Y < ?width) of
    true -> Vstep = ?maskWidth/2,
            Hstep = ?maskHeight/2,
     
	    SumFX = filterWeight(Input,
				 X,
				 Y, 
				 left(X, Vstep), 
				 top(Y, Hstep), 
				 Vstep, 
				 Hstep, 
				 0, 
				 bottom(X, Hstep, ?height), 
				 right(X, Vstep, ?width),
				 Msk),
            %% SumFX2 = SumFX + 0.5,
            
            %% Output = setElement((Y * ?width + X + 1), Input, SumFX),
            Output = setElement((Y * ?width + X + 2), Input, SumFX),

            convolution(Output, Msk, X, Y+1);
     false -> Input
    end.

top(A, Step) ->
    setTopLeft(A, Step).
bottom(A, Step, Cond) ->
    setBottomRight(A, Step, Cond).
left(A, Step) ->
    setTopLeft(A, Step).
right(A, Step, Cond) ->
    setBottomRight(A, Step, Cond).

setTopLeft(Z, Step) ->
    case Z < Step of
	true ->
	    0;
	false ->
	    (Z - Step)
    end.

setBottomRight(Z, Step, Comp) ->
    case ((Z + Step -1) >= Comp) of
	true ->
	    Comp;
	false ->
	    (Z + Step - 1)
    end.

-spec filterWeight([[bin_int()]], non_neg_integer(), non_neg_integer(), 
		   non_neg_integer(), non_neg_integer(), non_neg_integer(), 
		   non_neg_integer(), non_neg_integer(), non_neg_integer(), 
		   non_neg_integer(), [bin_int()]) -> non_neg_integer().
filterWeight(Input, X, Y, I, J, 
	     Vstep, Hstep, SumFX, Bottom, Right, Msk) when J =< Bottom ->
    MaskIndex = (J - (Y - Hstep)) * ?maskWidth + (I - (X - Vstep)),
    Index = J * ?width + I,
    %% io:fwrite(">~p ~p< ~n", [lists:nth(round(Index)+1, Input), lists:nth(round(MaskIndex)+1, Msk)]),
    SumFX2 = SumFX + (lists:nth(round(Index)+1, Input) * lists:nth(round(MaskIndex)+1, Msk)),
    filterWeight(Input,X,Y,I,J+1, Vstep, Hstep, SumFX2, Bottom, Right, Msk);
filterWeight(_,_,_,_,_,_,_,SumFX,_,_,_) -> 
    SumFX.
           
%% filterWeight1(Input, X,Y,I,J,Vstep,Hstep, SumFX, Bottom, Right,Msk) when I =< Right ->
%%     SumFx2 = filterWeight(Input,X,Y,I,J, Vstep, Hstep, SumFX, Bottom, Right,Msk),
%%     filterWeight1(Input,X,Y, I+1, J, Vstep, Hstep, SumFx2, Bottom, Right,Msk);
%% filterWeight1(_,_,_,_,_,_,_,SumFX,_,_,_) -> SumFX.
