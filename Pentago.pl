% The initialized board
% all positions are empty
initialize_game_board( Board) :-
      Board = b(e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e,e).


startUser():-
	initialize_game_board(B),
	play(user, x, B).

startComputer():-
	initialize_game_board(B).
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
     play(user, Next, NewBoard).

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
	% continue playing
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
addSign(B, S, X, Y, NewBoard).

%TODO
%rotate quater Q to direction R into RotatedBoard
rotateBoard(B, Q, R, RotatedBoard).

toggleSign(x, o).
toggleSign(o, x).

%returns the Sign S fer a position [X, Y] in Board B
getSign(B, X, Y, S) :-
        Num is ((X ) * 8) + Y,
        arg(Num, B, S).

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

