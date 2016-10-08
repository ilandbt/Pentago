%example
draw_board1() :-
    write('    1   2   3   4     5   6   7  8'), nl,
    write('   ----------------------------------'), nl,
    write('A |   |   |   |   |*|   |   |   |   |'), nl,
    write('   ----------------------------------'), nl,
    write('B |   |   |   |   |*|   |   |   |   |'), nl,
    write('   ----------------------------------'), nl,
    write('C |   |   |   |   |*|   |   |   |   |'), nl,
    write('   ----------------------------------'), nl,
    write('D |   |   |   |   |*|   |   |   |   |'), nl,
    write('   **********************************'), nl,
    write('E |   |   |   |   |*|   |   |   |   |'), nl,
    write('   ----------------------------------'), nl,
    write('F |   |   |   |   |*|   |   |   |   |'), nl,
    write('   ----------------------------------'), nl,
    write('G |   |   |   |   |*|   | 0 |   |   |'), nl,
    write('   ----------------------------------'), nl,
    write('H |   |   |   |   |*|   | X |   |   |'), nl,
    write('   ----------------------------------'), nl.

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

play(X, Y, V) :-
	pos(X,Y,'  '),!,
	retract(pos(X,Y,_)),
	atom_concat(V, ' ', Temp),
	assert(pos(X,Y,Temp)),
	d(),!
	;
	write('position taken!').
	

rotate(Q) :-
	Q = 'TL',!,rotateI(1,1,0,0)
	;
	Q = 'TR',!,rotateI(1,1,0,4)
	;
	Q = 'BL',!,rotateI(1,1,4,0)
	;
	Q = 'BR',rotateI(1,1,4,4).

rotateI(3, _, _, _):-!.
rotateI(I, J, Si, Sj):-
	rotateJ(I, J, Si, Sj),
	I1 is I + 1,
	rotateI(I1, J, Si, Sj).

rotateJ(_,3,_,_):-!.
rotateJ(I,J, Si, Sj):-
	switch(I,J,Si,Sj),
	J1 is J + 1,
	rotateJ(I, J1, Si, Sj).


switch(I, J, Si, Sj) :-
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
    assert(pos(I1i,J1j,V2)),
    assert(pos(T1i,I1j,V3)),
    assert(pos(T2i,T1j,V4)),
    assert(pos(J1i,T2j,V1)),!.

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
	
	
	
    
    

