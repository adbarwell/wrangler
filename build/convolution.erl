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

-include_lib("/Users/chrisb/ProgLang/erl_img/include/erl_img.hrl").

maskWidth() -> 8.
maskHeight() -> 8.

width() -> 400.
height() -> 400.

createRecord(R) ->    
    
    Pixels = createPixels(R, 1),
    ThePixMap = #erl_pixmap{top = 0,
                            left = 0, width = 64, height = 64, palette = undefined, format = r8g8b8a8, attributes=[], pixels  = Pixels},                                  
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
    
createPixels([],_) -> [];
createPixels([H|T], N) ->
    [{N, list_to_binary(H)} | createPixels(T, N+1)].
    
    
readImage(FileName) ->	
      {ok, _Img=#erl_image{format=F1, pixmaps=[PM]}} = erl_img:load(FileName),
      #erl_pixmap{pixels=Rows} =PM,
      R = lists:map(fun({A,B}) -> binary_to_list(B) end, Rows),
      {R, F1}.

createInput(Filename) ->
    {R, F1} = readImage(Filename),
    Msk = [0 || _ <- lists:seq(1, maskWidth()*maskHeight())],
    Val = 1.0 / (maskWidth() * 2.0 - 1.0),
    
    Y = maskHeight()/2,

    % Msk2 = setListElement(Msk, Y*maskWidth(), 0, Val, fun(N) -> (maskHeight()/2) * maskWidth() + N end),
    Msk2 = setListElement1(round(Y), 0, maskWidth(), Val, Msk),
   % io:fwrite("~p ~n", [Msk2]), 
    
    X = maskWidth()/2,
    % Msk3 = setListElement(Msk2, 0, 0, Val, fun(N) -> N * maskWidth() + (maskWidth()/2) end),
    Msk3 = setListElement2(round(X), 0, maskHeight(), Val, Msk2),
    
    {R, Msk3}.
 
setListElement1(Y, End, End, Val, List) -> List; 
setListElement1(Y,I, End, Val,List ) -> 
    NewList = setElement(Y*maskWidth()+(I+1), List, Val),
    setListElement1(Y, I+1, End, Val, NewList).
    
setListElement2(X,End, End, Val, List) -> List;
setListElement2(X,I, End, Val, List) ->
    % io:fwrite("~p~n", [(I+1)*maskWidth()+X]),
    NewList = setElement((I+1)*maskWidth()+X, List, Val),
    setListElement2(X, I+1, End, Val, NewList).
  
%setElement(1, [_|Rest], New) -> [New|Rest];
%setElement(I, [E|Rest], New) -> [E|setElement(I-1, Rest, New)].    
    
    
%setListElement([], A, B, C, D) -> [];
%setListElement([ L | Ls ], Pos, Pos, Val, Acc) -> [ Val | setListElement(Ls, Acc(Pos), Pos+1, Val, Acc) ];
%setListElement([ L | Ls ], Pos, N, Val, Acc) -> [ L | setListElement(Ls, Acc(N), N, Val, Acc) ].
  
  
convolution0(Input, Msk) ->
    convolution1(Input, Msk, 0, 0).    
    
    
convolution1(Input, Msk, X, Y) -> case (X < height()) of
                                    true -> Input2 = convolution(Input, Msk, X, Y),
                                            convolution1(Input2, Msk, X+1, Y);
                                    false -> Input
                                  end.
% convolution1(Input, Msk, X, Y) -> Input.
    
convolution(Input, Msk, X, Y)  -> 
   case (Y < width()) of
    true -> Vstep = maskWidth()/2,
            Hstep = maskHeight()/2,
    
            Left =  case (X < Vstep) of
                        true -> 0;
                        false -> (X- Vstep) 
                    end,
            Right = case ((X+Vstep - 1) >= width()) of
                        true -> width();
                        false -> (X + Vstep -1)
                    end,
            Top =   case (Y < Hstep) of
                        true -> 0;
                        false -> (Y - Hstep)
                    end,
            Bottom = case ((Y + Hstep - 1) >= height()) of
                        true -> height()-1;
                        false -> (Y+Hstep -1)
                     end,
            SumFX = filterWeight(Input,X,Y, Left, Top, Vstep, Hstep, 0, Bottom, Right,Msk),
            SumFX2 = SumFX + 0.5,
            
            % task->outpt[y*width + x] = (ushort) sumFX;
         %   io:fwrite("~p~n", [(Y*width()+X+1)]),
            Output = setElement((Y*width()+X+1), Input, SumFX),
            Output2 = setElement((Y*width()+X+2), Input, SumFX),
            convolution(Output2, Msk, X, Y+1);
     false -> Input
    end.
%convolution(Input, _, _, _) -> Input.

setElement(X, [], New) -> [];
setElement(1, [_|Rest], New) -> [New|Rest];
setElement(I, [E|Rest], New) -> [E|setElement(I-1, Rest, New)].

    
filterWeight1(Input, X,Y,I,J,Vstep,Hstep, SumFX, Bottom, Right,Msk) when I =< Right ->
    SumFx2 = filterWeight(Input,X,Y,I,J, Vstep, Hstep, SumFX, Bottom, Right,Msk),
    filterWeight1(Input,X,Y, I+1, J, Vstep, Hstep, SumFx2, Bottom, Right,Msk);
filterWeight1(_,_,_,_,_,_,_,SumFX,_,_,_) -> SumFX.
      
filterWeight(Input,X,Y,I,J, Vstep, Hstep, SumFX, Bottom, Right,Msk) when J =< Bottom ->
    MaskIndex = (J - (Y - Hstep)) * maskWidth() + (I - (X - Vstep)),
    Index = J * width() + I,
    io:fwrite(">~p ~p< ~n", [lists:nth(round(Index)+1, Input), lists:nth(round(MaskIndex)+1, Msk)]),
    SumFX2 = SumFX + (lists:nth(round(Index)+1, Input) * lists:nth(round(MaskIndex)+1, Msk)),
    filterWeight(Input,X,Y,I,J+1, Vstep, Hstep, SumFX2, Bottom, Right, Msk);
filterWeight(_,_,_,_,_,_,_,SumFX,_,_,_) -> SumFX.
           
