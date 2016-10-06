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
    draw_board(['A','B','C','D','E','F','G', 'H'], [1,2,3,4,5,6,7,8]),
    write('  ----------------------------------'), nl.

start_game():-
	retractall(pos(_, _, _)),
	init_rows(['A','B','C','D','E','F','G', 'H'], [1,2,3,4,5,6,7,8]),
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

draw_board(['E'|Rs], C) :-
	write('  **********************************'), nl,
	write('E'),
	draw_row('E', C, 1),
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
	
	
    
    

