d():-
    write('   1   2   3   4     5   6   7   8'), nl,
				% d( ['A','B','C','D','E','F','G', 'H', 'I'], [1,2,3,4,5,6,7,8,9]).
    draw_board([1,2,3,4,5,6,7,8], [1,2,3,4,5,6,7,8]),
    write('  ----------------------------------'), nl.

start_game():-
	retractall(pos(_, _, _)),
	init_rows([1,2,3,4,5,6,7,8], [1,2,3,4,5,6,7,8]).
	%d(),!.

init_rows([], _):- !.
init_rows([R|Rs], C) :-
	init_column(R, C),
	init_rows(Rs, C).


init_column(_, []):- !.
init_column(R,[C|Cs]) :-
	assert(pos(R,C,'  ')),
	init_column(R, Cs).

draw_board([], _) :- !.

draw_board([5|Rs], C) :-
	write('  **********************************'), nl,
	write(5),
	draw_row(5, C, 1),
	draw_board(Rs, C),!.
draw_board([R|Rs], C) :-
	write('  ----------------------------------'), nl,
	write(R),
	draw_row(R, C, 1),
	draw_board(Rs, C).


draw_row(_,[], _) :-
	write('|'),nl,!.

draw_row(R, [C| Cs], 5) :-
	write('|*'),
	write('| '),
	
	pos(R,C,Val),
	write(Val),
	draw_row(R, Cs, 6),!.
  
draw_row(R, [C| Cs], Index) :-
				%check if [R,C] is allocated -Yes: write allocated value, Not: write '  '(2 spaces).
	(
	% Index = 5, write('|*')
	%;
	write('| '),
	pos(R,C,Val),
	write(Val),
	Index1 is Index + 1,
	 draw_row(R, Cs, Index1)).

%play the value V in position (X,Y)
play(X, Y, V) :-
	pos(X,Y,'  '),!,
	retract(pos(X,Y,_)),
	atom_concat(V, ' ', Temp),
	assert(pos(X,Y,Temp)),
	d(),!.
	%;
	%write('position taken!').
	

%rotate quarter Q to direction D 
rotate(Q, D) :-
	Q = 'TL',!,rotateI(1,1,0,0, D)
	;
	Q = 'TR',!,rotateI(1,1,0,4,D)
	;
	Q = 'BL',!,rotateI(1,1,4,0, D)
	;
	Q = 'BR',rotateI(1,1,4,4,D).

rotateI(3, _, _, _,_):-!.
rotateI(I, J, Si, Sj,D):-
	rotateJ(I, J, Si, Sj,D),
	I1 is I + 1,
	rotateI(I1, J, Si, Sj,D).

rotateJ(_,3,_,_,_):-!.
rotateJ(I,J, Si, Sj,D):-
	switch(I,J,Si,Sj,D),
	J1 is J + 1,
	rotateJ(I, J1, Si, Sj,D).


switch(I, J, Si, Sj, D) :-
	T1i is 5-J + Si,
	T1j is 5-J + Sj,
	T2i is 5-I + Si,
	T2j is 5-I + Sj,
	J1i is J + Si,
	J1j is J + Sj,
	I1i is I + Si,
	I1j is I + Sj,
    pos(I1i,J1j,V1),
    pos(T1i, I1j, V2),
    pos(T2i, T1j, V3),
    pos(J1i, T2j, V4),
    retract(pos(I1i,J1j,_)),
    retract(pos(T1i,I1j,_)),
    retract(pos(T2i,T1j,_)),
    retract(pos(J1i,T2j,_)),
    (D = 'CW',!,
    assert(pos(I1i,J1j,V2)),
    assert(pos(T1i,I1j,V3)),
    assert(pos(T2i,T1j,V4)),
    assert(pos(J1i,T2j,V1))
    ;
    assert(pos(I1i,J1j,V4)),
    assert(pos(T1i,I1j,V1)),
    assert(pos(T2i,T1j,V2)),
     assert(pos(J1i,T2j,V3))),!.

%check if the Val makes a win verticaly
didWinVertical(_,_,_,4):-!,fail.
didWinVertical(X,Y,Val, Pos):-
	X1 is X-3+Pos,
	X2 is X-2+Pos,
	X3 is X-1+Pos,
	X4 is X+Pos,
	(pos(X1,Y,Val),!,
	pos(X2,Y,Val),
	pos(X3,Y,Val),
	pos(X4,Y,Val)
	;
	Pos2 is Pos + 1,
	didWinVertical(X,Y,Val,Pos2)),!.


%check if the Val makes a win horizontly
didWinHorizontal(_,_,_,4):-!,fail.
didWinHorizontal(X,Y,Val, Pos):-
	Y1 is Y-3+Pos,
	Y2 is Y-2+Pos,
	Y3 is Y-1+Pos,
	Y4 is Y+Pos,
	(pos(X,Y1,Val),!,
	pos(X,Y2,Val),
	pos(X,Y3,Val),
	pos(X,Y4,Val)
	;
	Pos2 is Pos + 1,
	didWinHorizontal(X,Y,Val,Pos2)),!.




didWinDiagonalLeftToRight(_,_,_,4):-!,fail.
didWinDiagonalLeftToRight(X,Y,Val, Pos):-
	Y1 is Y-3+Pos,
	Y2 is Y-2+Pos,
	Y3 is Y-1+Pos,
	Y4 is Y+Pos,
	X1 is X-3+Pos,
	X2 is X-2+Pos,
	X3 is X-1+Pos,
	X4 is X+Pos,
	(pos(X1,Y1,Val),!,
	pos(X2,Y2,Val),
	pos(X3,Y3,Val),
	pos(X4,Y4,Val)
	;
	Pos2 is Pos + 1,
	didWinDiagonalLeftToRight(X,Y,Val,Pos2)),!.

didWinDiagonalRightToLeft(_,_,_,4):-!,fail.
didWinDiagonalRightToLeft(X,Y,Val, Pos):-
	Y1 is Y+3+Pos,
	Y2 is Y+2+Pos,
	Y3 is Y+1+Pos,
	Y4 is Y+Pos,
	X1 is X-3+Pos,
	X2 is X-2+Pos,
	X3 is X-1+Pos,
	X4 is X+Pos,
	(pos(X1,Y1,Val),!,
	pos(X2,Y2,Val),
	pos(X3,Y3,Val),
	pos(X4,Y4,Val)
	;
	Pos2 is Pos + 1,
	didWinDiagonalRightToLeft(X,Y,Val,Pos2)),!.




test1():-
	start_game(),
    retract(pos(1,1,_)),
    retract(pos(1,6,_)),
    retract(pos(6,1,_)),
    retract(pos(6,6,_)),
    assert(pos(1,1,'X ')),
    assert(pos(1,6,'X ')),
    assert(pos(6,1,'X ')),
    assert(pos(6,6,'X ')),
    d(),!.

test2():-
	start_game(),
    retract(pos(1,1,_)),
    retract(pos(2,2,_)),
    retract(pos(3,3,_)),
    retract(pos(4,4,_)),
    assert(pos(1,1,'X ')),
    assert(pos(2,2,'X ')),
    assert(pos(3,3,'X ')),
    assert(pos(4,4,'X ')),
    d(),!.



test3():-
	start_game(),
    retract(pos(1,1,_)),
    retract(pos(1,2,_)),
    retract(pos(1,3,_)),
    retract(pos(1,4,_)),
    assert(pos(1,1,'X ')),
    assert(pos(1,2,'X ')),
    assert(pos(1,3,'X ')),
    assert(pos(1,4,'X ')),
    d(),!.
	
	
    
    

