%pits(player1, 6, 6, 6, 6, 6, 6)
pits(3).
move(Board, LastPit, SeedsInHand, NewBoard) :-
	step(Board, LastPit, SeedsInHand, NewBoard, NewPit, NewSeedsInHand).

step(Board, LastPit, 0, Board, LastPit, 0) :- !.	
step(Board, LastPit, SeedsInHand, NewBoard, NewPit, NewSeedsInHand) :-
	Board = Player1Pits/Player2Pits,
	next_pit(LastPit, LastPit1),
	LastPit1 = NewPitPlayer/NewPitNo,
	SeedsInHand1 is SeedsInHand - 1,
	(
		NewPitPlayer = player2,!,
		add_seed_to_pit(Player2Pits, NewPitNo, NewPlayer2Pits),
		Board1 = Player1Pits/NewPlayer2Pits
	;
		add_seed_to_pit(Player1Pits, NewPitNo, NewPlayer1Pits),
		Board1 = NewPlayer1Pits/Player2Pits
	),
	step(Board1, LastPit1, SeedsInHand1, NewBoard, NewPit, NewSeedsInHand).

next_pit(LastPitPlayer/LastPitNo, NextPitPlayer/NextPitNo) :-
	pits(PitsPerPlayer),
	(
		LastPitNo is PitsPerPlayer + 1, !, 
		next_player(LastPitPlayer, NextPitPlayer),
		NextPitNo = 1
	;
		NextPitPlayer = LastPitPlayer,
		NextPitNo is LastPitNo + 1
	)
	.

add_seed_to_pit(Pits, No, NewPits) :-
	ArgNo is No + 2,
	Pits =.. PitsList,
	change_list(PitsList, NewPitsList, ArgNo, add),
	NewPits =.. NewPitsList.

%change_list(L1, L2, N, add/empty).
change_list([H1|T1], [H2|T1], 1, Action) :- !,
	(
		Action = add,!,
		H2 is H1 + 1
	;
		H2 = 0
	).
change_list([H1|T1], [H1|T2], N, Action) :- !,
	N1 is N - 1,
	change_list(T1, T2, N1, Action).


next_player(player1, player2).
next_player(player2, player1).



move(Pos1, Pos2) :-
	select_valid_pit(Pos1, ValidPitNo, SeedsInHand).

%	.

%step/3 - determines the next step in the current move
%step(Pos1, Pos2, SeedsInHand) :-


% selects a valid pit for a player. non deterministic
select_valid_pit((P1_Pits-P1_Kalah-P2_Pits-P2_Kalah)/player1/Preferences, ValidPitNo, Seeds) :-
	select_pit(P1_Pits, ValidPitNo, Seeds).

select_valid_pit((P1_Pits-P1_Kalah-P2_Pits-P2_Kalah)/player2/Preferences, ValidPitNo, Seeds) :-
	select_pit(P2_Pits, ValidPitNo, Seeds).
	
select_pit([], _, _) :- fail.
select_pit([P|Ps], ValidPitNo, Seeds) :-
	select_pit([P|Ps], 1, ValidPitNo, Seeds).
select_pit([P|Ps], Current, ValidPitNo, Seeds) :-
	P > 0, ValidPitNo= Current, Seeds = P
	;
	Next is Current + 1,
	select_pit(Ps, Next, ValidPitNo, Seeds).
