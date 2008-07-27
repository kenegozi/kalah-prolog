		

min_to_move(player1/_/_).
max_to_move(player2/_/_).

is_kalah(K) :-
	pits(P),
	K is P + 1.
