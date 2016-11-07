% The initialized board
% all positions are empty
initialize_game_board( Board) :-
      Board = b(e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e).


startUser():-
	initialize_game_board(B),
	play(user, x, B).

startComputer():-
	initialize_game_board(B),
	play(computer, o, B).

%check if any player won the game
play(_, S, B) :-
	%check if there is a winner
	didWin(B, S),
	%print the board
	drawBoard(B),
	write(S),write('winner!!!').
	%S = x,!, write('You WON!!!!! ')
	%;
	%write('You lose :( '),!.

% Users move
play(user, S, B):-
	%print the board
	drawBoard(B),
	% get next play
	write('whats your next move? '),
	read(NextMove),
	% proccess the next move
	proccessMove(S, NextMove, B).

play(computer, Sign, Board) :-
     %alphabeta(Sign/Board, -100, 100, Next/NewBoard, _, 2),
	 toggleSign(Sign, NextSign),
     play(user, NextSign, Board).%should be NewBoard


%proccessing the moves
%procces move at location [X,Y] and rotate Quater Q in R Direction
proccessMove(_, stop, _):-!.
proccessMove(S, X-Y-Q-R, B) :-
	%add Sign
	validateMove(B, X, Y),
	addSign(B, S, X, Y, NewBoard),
	%rotate a quater of the board
	rotateBoard(NewBoard, Q, R, RotatedBoard),
	%switch player
	toggleSign(Sign, NextSign),
	play(computer, NextSign , RotatedBoard).



%input error from user
proccessMove(S, _, B) :-
	write('input error, please enter your move again.'),nl,
	play(user, S, B).

%validate move
validateMove(B, X, Y) :-
	getSign(B, X, Y, e).

%TODO
%add Sign S at location X, Y in to newBoard
addSign(B, S, X, Y, NewBoard) :-
     Num is ((X -1) * 8) + Y-1,
	 B =.. [b|BoardList],
	 replace(BoardList, Num, S, TempList),
	 NewBoard =.. [b|TempList],write(new),write(NewBoard),nl.



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
	RotatedBoard = b(X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20,X21,X22,X23,X24,X25,X26,X27,X28,X29,X30,X31,X32,X33,X34,X35,X36,X37,X53,X45,X61,X41,X42,X43,X44,X62,X54,X46,X38,X49,X50,X51,X52,X63,X55,X47,X39,X57,X58,X59,X60,X64,X56,X48,X40).

toggleSign(x, o).
toggleSign(o, x).

%returns the Sign S for a position [X, Y] in Board B
getSign(B, X, Y, S) :-
        Num is ((X ) * 8) + Y,
        arg(Num, B, S).


replace([_|T], 0, X, [X|T]).
replace([H|T], I, X, [H|R]):- I > -1, NI is I-1, replace(T, NI, X, R), !.
replace(L, _, _, L).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fourLists(L,S) :-
    L = [[e,S,S,S,S],[S,e,S,S,S],[S,S,e,S,S],[S,S,S,e,S],[S,S,S,S,e]].
threeLists(L,S):-
    L=[[e,e,S,S,S],[e,S,S,S,e],[e,e,S,S,S],[S,e,e,S,S],[S,S,e,e,S],[S,e,S,e,S]].
twoLists(L,S):-
    L=[[e,e,e,S,S],[e,e,S,S,e],[e,S,S,e,e],[S,S,e,e,e], [S,e,S,e,e],[S,e,e,S,e],[S,e,e,e,S], [e,S,e,S,e],[e,S,e,e,S],[e,e,S,e,S]].
oneLists(L,S):-
    L=[[e,e,e,e,S],[e,e,e,S,e],[e,e,S,e,e],[e,S,e,e,e],[S,e,e,e,e]].



listSum([], 0).
listSum([H|T], Sum) :-
   sum_list(T, Rest),
   Sum is H + Rest.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%check if S run on board B
didWin(B, S) :-
	resultChecker(B, [S,S,S,S,S], _).


resultChecker(B, Compare, Length) :- 
    findall(T, (gapv(Output, B),member(T, Output) ,T=Compare), Result),
    length(Result, Length),
    Length > 0.

gapv(Output, Board):-
    findall(Vals, (groupsOfWinningFive(Groups),member(Group, Groups), gpv(Group, Vals, Board)), Output).
gpv(Input, Output, Board):-
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

drawPosition(B, X, Y, D) :-
	getSign(B, X, Y, S),
	signMap(S, D).

%TODO
drawBoard(B) :-
	write('   1   2   3   4     5   6   7   8'), nl,
	drawBoard(B, 1, 1, 8, 8),
	write('  ----------------------------------'), nl, !.

drawBoard(_, _, Y, _, Yend):-
	Temp is Yend + 1,
	Temp = Y,!.
drawBoard(B, X, Y, Xend, Yend) :-
    Temp is Yend/2+1,
    Y = Temp,
    write('  **********************************'), nl,
	write(X),
    drawRow(B, 0,Y, Xend),
    Xtemp is X + 1,
    Ytemp is Y + 1,
    drawBoard(B, Xtemp, Ytemp, Xend,Yend).
    

drawBoard(B, X, Y, Xend, Yend) :-
	write('  ----------------------------------'), nl,
	write(X),
    drawRow(B, 0,Y, Xend),
    Xtemp is X + 1,
    Ytemp is Y + 1,
    drawBoard(B, Xtemp, Ytemp, Xend,Yend).
drawRow(_, Xend, _, Xend) :-
    write('|'),nl,!.
drawRow(B, X, Y, Xend) :-
    Temp is Xend/2,
    X = Temp,
    write('|*'),
	write('| '),
	%pos(R,C,Val),
	%write(Val),
	drawPosition(B, X, Y, D),
	write(D),
	Xnext is X + 1,
    drawRow(B, Xnext,Y,  Xend).
drawRow(B, X, Y,Xend) :-
    write('| '),
	drawPosition(B, X, Y, D),
	write(D),
	Xnext is X + 1,
    drawRow(B, Xnext, Y,Xend).

