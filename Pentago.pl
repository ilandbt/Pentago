% Programmer: Gil Osher
% File Name: Pentago.pl
% Description: Pentago is a two-player abstract strategy game
%				The game is played on a 8x8 board divided into four 4x4 sub-boards (or quadrants). 
%				Taking turns, the two players place a marble of their shape (x or o) onto an unoccupied space on the board,
%				and then rotate one of the sub-boards by 90 degrees either clockwise or anti-clockwise.
%				A player WINS by getting five of their shapes in a vertical, horizontal or diagonal row. 
%
%
% Synopsys: 
%			startUser. - For the user to play the first move
%           startComputer. - For the computer to play the first move
%
%           each turn you need to enter your move in the next format
%			Column-Row-SubBoard-RotationDirection
%
%			column -> [1-8]
%			row -> [1-8]
%			subBoard -> [tl,tr,bl,br] - (explanation: tl -> top left, tr -> top right, bl - bottom left, br - bottom right)
%			rotationDirection - [cw,ccw] - (explanation: cw -> clockwise, ccw -> counter clockwise)
%	
%           [row, column] is the position of the marble on the board (explanation: row -> from top to bottom, column -> from left to right)
%			
%			EXAMPLE: play: 3-6-br-cw
%			
%
%				   1   2   3   4     5   6   7   8				   	 1   2   3   4     5   6   7   8
%				  ----------------------------------				----------------------------------
%				1|   |   |   |   |*|   |   |   |   |			  1|   |   |   |   |*|   |   |   |   |
%				  ----------------------------------				----------------------------------
%				2|   |   |   |   |*|   |   |   |   |			  2|   |   |   |   |*|   |   |   |   |
%				  ----------------------------------				----------------------------------
%				3|   |   |   |   |*|   |   |   |   |			  3|   |   |   |   |*|   |   |   |   |
%				  ----------------------------------				----------------------------------
%	 Before ->  4|   |   |   |   |*|   |   |   |   |	After ->  4|   |   |   |   |*|   |   |   |   |
%				  **********************************				**********************************
%				5|   |   |   |   |*|   |   |   |   |			  5|   |   |   |   |*|   |   |   |   |
%				  ----------------------------------				----------------------------------
%				6|   |   |   |   |*|   |   |   |   |			  6|   |   | X |   |*|   |   |   |   |
%				  ----------------------------------				----------------------------------
%				7|   |   |   |   |*|   |   |   |   |			  7|   |   |   |   |*|   |   |   |   |
%				 ----------------------------------					----------------------------------
%				8|   |   |   |   |*|   |   |   | O |			  8|   |   |   |   |*| O |   |   |   |
% 				 ----------------------------------					----------------------------------
%
%
%			In each turn you can call -> stop. <- for aborting the game.	
%			In each tuen you can call -> help. <- for the manual.	
%
%
  








% The initialized board
% all positions are empty
initialize_game_board( Board) :-
      Board = b(e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e).

clear :-
      retractall(max_to_move(_)),
      retractall(min_to_move(_)),!.

startUser():-
	clear,
	initialize_game_board(B),
     assert(min_to_move(x-_)),assert(max_to_move(o-_)),
	play(user, x, B).

startComputer():-
	clear,
	initialize_game_board(Board),
	assert(min_to_move(o-_)),assert(max_to_move(x-_)),
	play(computer, o, Board).

%check if any player won the game

play(_, Sign, Board) :-
	%check if there is a winner
	(didWin(Board, Sign), clear,drawBoard(Board), drawWinner(Sign)
	;
	toggleSign(Sign, OtherSign),
	didWin(Board, OtherSign),clear,drawBoard(Board), drawWinner(OtherSign)).

% Users move
play(user, Sign, Board):-

	drawBoard(Board),
	% get next play
	write('whats your next move? '),
	read(NextMove),
	% proccess the next move
	proccessMove(Sign, NextMove, Board).

play(computer, Sign, Board) :- 
	drawBoard(Board),
    alphabeta(Sign-Board, -1000,1000,NextSign-NextBoard,_,1),
    play(user, NextSign, NextBoard).


%proccessing the moves
%procces move at location [X,Y] and rotate Quater Q in R Direction
proccessMove(_, stop, _):-!.
proccessMove(Sign,help,Board):-
	help,
	play(user, Sign, Board).

proccessMove(S, X-Y-Q-R, B) :-
	%add Sign
	validateMove(B, X, Y),
	addSign(B, S, X, Y, NewBoard),
	%rotate a quater of the board
	rotateBoard(NewBoard, Q, R, RotatedBoard),
	%switch player
	toggleSign(S, NextSign),

	play(computer, NextSign , RotatedBoard).

proccessMove(S, X-Y-_-_, B) :-
	not(validateMove(B, X, Y)),
	write('Position taken, please enter new position or type help. for instructions.'),nl,
	play(user, S, B).


%input error from user
proccessMove(S, _, B) :-
	write('input error, please enter your move again.'),nl,
	play(user, S, B).

%validate move
validateMove(B, X, Y) :-
	%getSign(B, X, Y, e),
	Num is ((X -1) * 8) + Y-1+1,
    arg(Num, B, e).

%add Sign S at location X, Y in to newBoard
addSign(B, S, X, Y, NewBoard) :-
     Num is ((X -1) * 8) + Y-1,
	 B =.. [b|BoardList],
	 replace(BoardList, Num, S, TempList),
	 NewBoard =.. [b|TempList].%write(new),write(NewBoard),nl.



%rotate quater Q to direction R into RotatedBoard
%rotateBoard(B, Q, R, RotatedBoard).

rotateBoard(B, tl, cw, RotatedBoard) :-!,
	B = b(X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20,X21,X22,X23,X24,X25,X26,X27,X28,X29,X30,X31,X32,X33,X34,X35,X36,X37,X38,X39,X40,X41,X42,X43,X44,X45,X46,X47,X48,X49,X50,X51,X52,X53,X54,X55,X56,X57,X58,X59,X60,X61,X62,X63,X64),
	RotatedBoard = b(X4,X12,X20,X28,X5,X6,X7,X8,X3,X11,X19,X27,X13,X14,X15,X16,X2,X10,X18,X26,X21,X22,X23,X24,X1,X9,X17,X25,X29,X30,X31,X32,X33,X34,X35,X36,X37,X38,X39,X40,X41,X42,X43,X44,X45,X46,X47,X48,X49,X50,X51,X52,X53,X54,X55,X56,X57,X58,X59,X60,X61,X62,X63,X64).

rotateBoard(B, bl, cw, RotatedBoard) :-!,
	B = b(X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20,X21,X22,X23,X24,X25,X26,X27,X28,X29,X30,X31,X32,X33,X34,X35,X36,X37,X38,X39,X40,X41,X42,X43,X44,X45,X46,X47,X48,X49,X50,X51,X52,X53,X54,X55,X56,X57,X58,X59,X60,X61,X62,X63,X64),
	RotatedBoard = b(X1,X2,X3,X4,X8,X16,X24,X32,X9,X10,X11,X12,X7,X15,X23,X31,X17,X18,X19,X20,X6,X14,X22,X30,X25,X26,X27,X28,X5,X13,X21,X29,X33,X34,X35,X36,X37,X38,X39,X40,X41,X42,X43,X44,X45,X46,X47,X48,X49,X50,X51,X52,X53,X54,X55,X56,X57,X58,X59,X60,X61,X62,X63,X64).

rotateBoard(B, tr, cw, RotatedBoard) :-!,
	B = b(X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20,X21,X22,X23,X24,X25,X26,X27,X28,X29,X30,X31,X32,X33,X34,X35,X36,X37,X38,X39,X40,X41,X42,X43,X44,X45,X46,X47,X48,X49,X50,X51,X52,X53,X54,X55,X56,X57,X58,X59,X60,X61,X62,X63,X64),
	RotatedBoard = b(X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20,X21,X22,X23,X24,X25,X26,X27,X28,X29,X30,X31,X32,X36,X44,X52,X60,X37,X38,X39,X40,X35,X43,X51,X59,X45,X46,X47,X48,X34,X42,X50,X58,X53,X54,X55,X56,X33,X41,X49,X57,X61,X62,X63,X64).

rotateBoard(B, br, cw, RotatedBoard) :-!,
	B = b(X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20,X21,X22,X23,X24,X25,X26,X27,X28,X29,X30,X31,X32,X33,X34,X35,X36,X37,X38,X39,X40,X41,X42,X43,X44,X45,X46,X47,X48,X49,X50,X51,X52,X53,X54,X55,X56,X57,X58,X59,X60,X61,X62,X63,X64),
	RotatedBoard = b(X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20,X21,X22,X23,X24,X25,X26,X27,X28,X29,X30,X31,X32,X33,X34,X35,X36,X40,X48,X56,X64,X41,X42,X43,X44,X39,X47,X55,X63,X49,X50,X51,X52,X38,X46,X54,X62,X57,X58,X59,X60,X37,X45,X53,X61).

rotateBoard(B, tl, ccw, RotatedBoard) :-!,
	B = b(X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20,X21,X22,X23,X24,X25,X26,X27,X28,X29,X30,X31,X32,X33,X34,X35,X36,X37,X38,X39,X40,X41,X42,X43,X44,X45,X46,X47,X48,X49,X50,X51,X52,X53,X54,X55,X56,X57,X58,X59,X60,X61,X62,X63,X64),
	RotatedBoard = b(X25,X17,X9,X1,X5,X6,X7,X8,X26,X18,X10,X2,X13,X14,X15,X16,X27,X19,X11,X3,X21,X22,X23,X24,X28,X20,X12,X4,X29,X30,X31,X32,X33,X34,X35,X36,X37,X38,X39,X40,X41,X42,X43,X44,X45,X46,X47,X48,X49,X50,X51,X52,X53,X54,X55,X56,X57,X58,X59,X60,X61,X62,X63,X64).

rotateBoard(B, bl, ccw, RotatedBoard) :-!,
	B = b(X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20,X21,X22,X23,X24,X25,X26,X27,X28,X29,X30,X31,X32,X33,X34,X35,X36,X37,X38,X39,X40,X41,X42,X43,X44,X45,X46,X47,X48,X49,X50,X51,X52,X53,X54,X55,X56,X57,X58,X59,X60,X61,X62,X63,X64),
	RotatedBoard = b(X1,X2,X3,X4,X29,X21,X13,X5,X9,X10,X11,X12,X30,X22,X14,X6,X17,X18,X19,X20,X31,X23,X15,X7,X25,X26,X27,X28,X32,X24,X16,X8,X33,X34,X35,X36,X37,X38,X39,X40,X41,X42,X43,X44,X45,X46,X47,X48,X49,X50,X51,X52,X53,X54,X55,X56,X57,X58,X59,X60,X61,X62,X63,X64).

rotateBoard(B, tr, ccw, RotatedBoard) :-!,
	B = b(X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20,X21,X22,X23,X24,X25,X26,X27,X28,X29,X30,X31,X32,X33,X34,X35,X36,X37,X38,X39,X40,X41,X42,X43,X44,X45,X46,X47,X48,X49,X50,X51,X52,X53,X54,X55,X56,X57,X58,X59,X60,X61,X62,X63,X64),
	RotatedBoard = b(X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20,X21,X22,X23,X24,X25,X26,X27,X28,X29,X30,X31,X32,X57,X49,X41,X33,X37,X38,X39,X40,X58,X50,X42,X34,X45,X46,X47,X48,X59,X51,X43,X35,X53,X54,X55,X56,X60,X52,X44,X36,X61,X62,X63,X64).

rotateBoard(B, br, ccw, RotatedBoard) :-!,
	B = b(X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20,X21,X22,X23,X24,X25,X26,X27,X28,X29,X30,X31,X32,X33,X34,X35,X36,X37,X38,X39,X40,X41,X42,X43,X44,X45,X46,X47,X48,X49,X50,X51,X52,X53,X54,X55,X56,X57,X58,X59,X60,X61,X62,X63,X64),
	RotatedBoard = b(X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20,X21,X22,X23,X24,X25,X26,X27,X28,X29,X30,X31,X32,X33,X34,X35,X36,X61,X53,X45,X37,X41,X42,X43,X44,X62,X54,X46,X38,X49,X50,X51,X52,X63,X55,X47,X39,X57,X58,X59,X60,X64,X56,X48,X40).

toggleSign(x, o).
toggleSign(o, x).

%returns the Sign S for a position [X, Y] in Board B
getSign(B, X, Y, S) :-
        Num is ((X -1) * 8) + Y-1,
        arg(Num, B, S).


replace([_|T], 0, X, [X|T]).
replace([H|T], I, X, [H|R]):- I > -1, NI is I-1, replace(T, NI, X, R), !.
replace(L, _, _, L).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  -----      alpha beta      -----  %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%get all available moves in Board B
moves(S-B, Moves) :-
	Rotations=[cw,ccw],
	Locations=[tl,bl,tr,br],
	setof([X,Y,I,J],Index^(arg(Index,B,_), member(I, Rotations), member(J, Locations), X is Index//8+1,Y is mod(Index,8)), AllMoves),
	findall(Sign-Board, (member([X,Y,R,Q], AllMoves),addSign(B, S, X, Y, NewBoard),rotateBoard(NewBoard,Q,R,Board),toggleSign(S, Sign)), Moves),!.


%TODO: implement
staticval(S-B, Val) :-
	didWin(B,S),!,Val = 1000
	;
	toggleSign(S, NextSign),didWin(B, NextSign),!, Val = 1000
	;
	toggleSign(S, NextSign),
	checkFour(B,S,ValFour),
	checkThree(B,S,ValThree),
	checkTwo(B,S,ValTwo),
	%checkOne(B,S,ValOne),
	checkFour(B,NextSign,ValFourT),
	checkThree(B,NextSign,ValThreeT),
	checkTwo(B,NextSign,ValTwoT),
	Val is ValFour*4 + ValThree*3 + ValTwo*2 - ValFourT - ValThreeT - ValTwoT.


alphabeta( Pos, Alpha, Beta, GoodPos, Val, Depth) :-
          	Depth > 0, moves( Pos, PosList), !,
           boundedbest( PosList, Alpha, Beta, GoodPos, Val, Depth);
           staticval( Pos, Val).        % Static value of Pos

boundedbest( [Pos|PosList], Alpha, Beta, GoodPos, GoodVal, Depth) :-
             Depth1 is Depth - 1,
             alphabeta( Pos, Alpha, Beta, _, Val, Depth1),
             goodenough( PosList, Alpha, Beta, Pos, Val, GoodPos, GoodVal, Depth).

goodenough( [], _, _, Pos, Val, Pos, Val, _) :- !.     % No other candidate

goodenough( _, Alpha, Beta, Pos, Val, Pos, Val, _) :-
            min_to_move( Pos), Val > Beta, !;       % Maximizer attained upper bound
            max_to_move( Pos), Val < Alpha, !.      % Minimizer attained lower bound

goodenough( PosList, Alpha, Beta, Pos, Val, GoodPos, GoodVal, Depth) :-
            newbounds( Alpha, Beta, Pos, Val, NewAlpha, NewBeta),        % Refine bounds
            boundedbest( PosList, NewAlpha, NewBeta, Pos1, Val1, Depth),
            betterof( Pos, Val, Pos1, Val1, GoodPos, GoodVal).

newbounds( Alpha, Beta, Pos, Val, Val, Beta) :-
           min_to_move( Pos), Val > Alpha, !.        % Maximizer increased lower bound

newbounds( Alpha, Beta, Pos, Val, Alpha, Val) :-
           max_to_move( Pos), Val < Beta, !.         % Minimizer decreased upper bound

newbounds( Alpha, Beta, _, _, Alpha, Beta).          % Otherwise bounds unchanged

betterof( Pos, Val, _, Val1, Pos, Val) :-         % Pos better then Pos1
          min_to_move( Pos), Val > Val1, !;
          max_to_move( Pos), Val < Val1, !.

betterof( _, _, Pos1, Val1, Pos1, Val1).             % Otherwise Pos1 better


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fourLists(L,S) :-
    L = [[e,S,S,S,S],[S,e,S,S,S],[S,S,e,S,S],[S,S,S,e,S],[S,S,S,S,e]].
threeLists(L,S):-
    L=[[e,e,S,S,S],[e,S,S,S,e],[e,e,S,S,S],[S,e,e,S,S],[S,S,e,e,S],[S,e,S,e,S]].
twoLists(L,S):-
    L=[[e,e,e,S,S],[e,e,S,S,e],[e,S,S,e,e],[S,S,e,e,e], [S,e,S,e,e],[S,e,e,S,e],[S,e,e,e,S], [e,S,e,S,e],[e,S,e,e,S],[e,e,S,e,S]].
oneLists(L,S):-
    L=[[e,e,e,e,S],[e,e,e,S,e],[e,e,S,e,e],[e,S,e,e,e],[S,e,e,e,e]].

checkFour(Board,S,Count):-
    findall(X,(fourLists(Lists, S), member(List, Lists), resultChecker(Board,List,X)),Res),
    listSum(Res,Count).

checkThree(Board,S,Count):-
    findall(X,(threeLists(Lists, S), member(List, Lists), resultChecker(Board,List,X)),Res),
    listSum(Res,Count).

checkTwo(Board,S,Count):-
    findall(X,(twoLists(Lists, S), member(List, Lists), resultChecker(Board,List,X)),Res),
    listSum(Res,Count).

checkOne(Board,S,Count):-
    findall(X,(oneLists(Lists, S), member(List, Lists), resultChecker(Board,List,X)),Res),
    listSum(Res,Count).

listSum([], 0).
listSum([H|T], Sum) :-
   listSum(T, Rest),
   Sum is H + Rest.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%check if S run on board B
didWin(Board, Sign) :-
	resultChecker(Board, [Sign,Sign,Sign,Sign,Sign], _).


resultChecker(Board, Compare, Length) :- 
    findall(T, (getAllPositionsValues(Output, Board),member(T, Output) ,T=Compare), Result),
    length(Result, Length),
    Length > 0.

%output - list of lists of all the values in board of all winnig options.
getAllPositionsValues(Output, Board):-
    findall(Vals, (groupsOfWinningFive(Groups),member(Group, Groups), getPositionsValues(Group, Vals, Board)), Output).

%input - list of positions, output - list of values at position in board
getPositionsValues(Input, Output, Board):-
    findall(S, (member(Index, Input),arg(Index, Board, S)),Output).

groupsOfWinningFive(L) :-
	L = [[1,2,3,4,5], [2,3,4,5,6], [3,4,5,6,7], [4,5,6,7,8], 
	[9,10,11,12,13], [10,11,12,13,14], [11,12,13,14,15], [12,13,14,15,16],
  	[17,18,19,20,21], [18,19,20,21,22], [19,20,21,22,23], [20,21,22,23,24],
  	[25,26,27,28,29], [26,27,28,29,30], [27,28,29,30,31], [28,29,30,31,32],
  	[33,34,35,36,37], [34,35,36,37,38] ,[35,36,37,38,39] ,[36,37,38,39,40],
  	[41,42,43,44,45],[42,43,44,45,46],[43,44,45,46,47],[44,45,46,47,48],
  	[49,50,51,52,53],[50,51,52,53,54],[51,52,53,54,55],[52,53,54,55,56],
  	[57,58,59,60,61],[58,59,60,61,62],[59,60,61,62,63],[60,61,62,63,64],

  	[1,9,17,25,33],[9,17,25,33,41],[17,25,33,41,49],[25,33,41,49,57],
  	[2,10,18,26,34],[10,18,26,34,42],[18,26,34,42,50],[26,34,42,50,58],
  	[3,11,19,27,35],[11,19,27,35,43],[19,27,35,43,51],[27,35,43,51,59],
  	[4,12,20,28,36],[12,20,28,36,44],[20,28,36,44,52],[28,36,44,52,60],
  	[5,13,21,29,37],[13,21,29,37,45],[21,29,37,45,53],[29,37,45,53,61],
  	[6,14,22,30,38],[14,22,30,38,46],[22,30,38,46,54],[30,38,46,54,62],
  	[7,15,23,31,39],[15,23,31,39,47],[23,31,39,47,55],[31,39,47,55,63],
  	[8,16,24,32,40],[16,24,32,40,48],[24,32,40,48,56],[32,40,48,56,64] ,

  	[25,34,43,52,61],
  	[17,26,35,44,53],[26,35,44,53,62],
  	[9,18,27,36,45],[18,27,36,45,54],[27,36,45,54,63],
  	[1,10,19,28,37],[10,19,28,37,46],[19,28,37,46,55],[28,37,46,55,64],
  	[2,11,20,29,38],[11,20,29,38,47],[20,29,38,47,56],
  	[3,12,21,30,39],[12,21,30,39,48],
  	[4,13,22,31,40],

  	[5,12,19,26,33],
  	[6,13,20,27,34],[13,20,27,34,41],
  	[7,14,21,28,35],[14,21,28,35,42],[21,28,35,42,49],
  	[8,15,22,29,36],[15,22,29,36,43],[22,29,36,43,50],[29,36,43,50,57],
  	[16,23,30,37,44],[23,30,37,44,51],[30,37,44,51,58],
  	[24,31,38,45,52],[31,38,45,52,59],
  	[32,39,46,53,60] ].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  -----  drawing predicates  -----  %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

signMap(e, '  ').
signMap(x, 'X ').
signMap(o, 'O ').

drawPosition(Board, X, Y, Output) :-
	getSign(Board, X, Y, Sign),
	signMap(Sign, Output).

drawBoard(Board) :-
	write('   1   2   3   4     5   6   7   8'), nl,
	drawBoard(Board, 1, 1, 8, 8),
	write('  ----------------------------------'), nl, !.

drawBoard(_, _, Y, _, Yend):-
	Temp is Yend + 1,
	Temp = Y,!.
drawBoard(Board, X, Y, Xend, Yend) :-
    Temp is Yend/2+1,
    Y = Temp,
    write('  **********************************'), nl,
	write(X),
    drawRow(Board, 0,Y, Xend),
    Xtemp is X + 1,
    Ytemp is Y + 1,
    drawBoard(Board, Xtemp, Ytemp, Xend,Yend).
    

drawBoard(Board, X, Y, Xend, Yend) :-
	write('  ----------------------------------'), nl,
	write(X),
    drawRow(Board, 0,Y, Xend),
    Xtemp is X + 1,
    Ytemp is Y + 1,
    drawBoard(Board, Xtemp, Ytemp, Xend,Yend).
drawRow(_, Xend, _, Xend) :-
    write('|'),nl,!.
drawRow(Board, X, Y, Xend) :-
    Temp is Xend/2,
    X = Temp,
    write('|*'),
	write('| '),
    X1 is X + 1,
    Y1 is Y + 1,
	drawPosition(Board, X1, Y1, D),
	write(D),
	Xnext is X + 1,
    drawRow(Board, Xnext,Y,  Xend).

drawRow(Board, X, Y,Xend) :-
    write('| '),
    X1 is X + 1,
    Y1 is Y + 1,
	drawPosition(Board, X1, Y1, D),
	write(D),
	Xnext is X + 1,
    drawRow(Board, Xnext, Y,Xend).

drawWinner(x) :-
	drawWin,!.

drawWinner(o) :-
	drawLose,!.

 drawWin:-
 	write(' __  __     ______     __  __        __     __     __     __   __   '),nl,
 	write('/\\ \\_\\ \\   /\\  __ \\   /\\ \\/\\ \\      /\\ \\  _ \\ \\   /\\ \\   /\\ "-.\\ \\   '),nl,
 	write('\\ \\____ \\  \\ \\ \\/\\ \\  \\ \\ \\_\\ \\     \\ \\ \\/ ".\\ \\  \\ \\ \\  \\ \\ \\-.  \\   '),nl,
 	write(' \\/\\_____\\  \\ \\_____\\  \\ \\_____\\     \\ \\__/".~\\_\\  \\ \\_\\  \\ \\_\\\\"\\_\\   '),nl,
 	write('  \\/_____/   \\/_____/   \\/_____/      \\/_/   \\/_/   \\/_/   \\/_/ \\/_/  '),nl.

drawLose :-
	write(' __  __     ______     __  __        __         ______     ______     ______   '),nl,
	write('/\\ \\_\\ \\   /\\  __ \\   /\\ \\/\\ \\      /\\ \\       /\\  __ \\   /\\  ___\\   /\\  ___\\  '),nl,
	write('\\ \\____ \\  \\ \\ \\/\\ \\  \\ \\ \\_\\ \\     \\ \\ \\____  \\ \\ \\/\\ \\  \\ \\___  \\  \\ \\  __\\  '),nl,
	write(' \\/\\_____\\  \\ \\_____\\  \\ \\_____\\     \\ \\_____\\  \\ \\_____\\  \\/\\_____\\  \\ \\_____\\'),nl,
	write('  \\/_____/   \\/_____/   \\/_____/      \\/_____/   \\/_____/   \\/_____/   \\/_____/'),nl.


help :-
	nl,nl,
	write('---------------------------------------------------------------------------------------------------------------------------------------------------------------'),nl,
	write('HELP:'),nl,
	write('Each turn you need to enter your move in the next format'),nl,
	write('Column-Row-SubBoard-RotationDirection'),nl,nl,
	write('Column -> [1-8]'),nl,
	write('Row -> [1-8]'),nl,
	write('SubBoard -> [tl,tr,bl,br] - (explanation: tl -> top left, tr -> top right, bl - bottom left, br - bottom right)'),nl,
	write('RotationDirection - [cw,ccw] - (explanation: cw -> clockwise, ccw -> counter clockwise)'),nl,nl,
	write('[row, column] is the position of the marble on the board (explanation: row -> from top to bottom, column -> from left to right)'),nl,nl,nl,
	write('		EXAMPLE:'),nl,
	write('		play: 3-6-br-cw'),nl,
	write('				   1   2   3   4     5   6   7   8			     1   2   3   4     5   6   7   8'),nl,
	write('				  ----------------------------------			    ----------------------------------'),nl,
	write('				1|   |   |   |   |*|   |   |   |   |			  1|   |   |   |   |*|   |   |   |   |'),nl,
	write('				  ----------------------------------			    ----------------------------------'),nl,
	write('				2|   |   |   |   |*|   |   |   |   |			  2|   |   |   |   |*|   |   |   |   |'),nl,
	write('				  ----------------------------------			    ----------------------------------'),nl,
	write('				3|   |   |   |   |*|   |   |   |   |			  3|   |   |   |   |*|   |   |   |   |'),nl,
	write('				  ----------------------------------			    ----------------------------------'),nl,
	write('			Before ->  4|   |   |   |   |*|   |   |   |   |		   After ->  4|   |   |   |   |*|   |   |   |   |'),nl,
	write('				  **********************************			    **********************************'),nl,
	write('				5|   |   |   |   |*|   |   |   |   |			  5|   |   |   |   |*|   |   |   |   |'),nl,
	write('				  ----------------------------------			    ----------------------------------'),nl,
	write('				6|   |   |   |   |*|   |   |   |   |			  6|   |   | X |   |*|   |   |   |   |'),nl,
	write('				  ----------------------------------			    ----------------------------------'),nl,
	write('				7|   |   |   |   |*|   |   |   |   |			  7|   |   |   |   |*|   |   |   |   |'),nl,
	write('				  ----------------------------------			    ----------------------------------'),nl,
	write('				8|   |   |   |   |*|   |   |   | O |			  8|   |   |   |   |*| O |   |   |   |'),nl,
	write('				  ----------------------------------			    ----------------------------------'),nl,nl,nl,
	write(		'In each turn you can call -> stop. <- for aborting the game.'),nl,nl,nl,
	write('---------------------------------------------------------------------------------------------------------------------------------------------------------------'),nl,nl,nl.


