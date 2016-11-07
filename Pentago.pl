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
/*
%check if any player won the game
play(_, S, B) :-
	%check if there is a winner
	didWin(S,B),
	%print the board
	drawBoard(B),
	S = x,!, write('You WON!!!!! ')
	;
	write('You lose :( '),!.
*/

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

%TODO
%check if S run on board B
didWin(S,B).

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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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

