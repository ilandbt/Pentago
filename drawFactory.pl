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
	init_rows([1,2,3,4,5,6,7,8], [1,2,3,4,5,6,7,8]),
	d(),!.

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

rotateI(3, _):-!.
rotateI(I, J):-
	rotateJ(I, J),
	I1 is I + 1,
	rotateI(I1, J).

rotateJ(_,3):-!.
rotateJ(I,J):-
	switch(I,J),
	J1 is J + 1,
	rotateJ(I, J1).


switch(I, J) :-
    T1 is 5-J,
    T2 is 5-I,
    pos(I,J,V1),
    pos(T1, I, V2),
    pos(T2, T1, V3),
    pos(J, T2, V4),
    retract(pos(I,J,_)),
    retract(pos(T1,I,_)),
    retract(pos(T2,T1,_)),
    retract(pos(J,T2,_)),
    assert(pos(I,J,V2)),
    assert(pos(T1,I,V3)),
    assert(pos(T2,T1,V4)),
    assert(pos(J,T2,V1)),!.
	
	
    
    

