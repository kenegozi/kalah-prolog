/***********************************************************************
	Program	:	Kalah game in PROLOG
	Written by	:	Ken Egozi
	File		:	moves.pl
	Description	:	moving between states
***********************************************************************/

move(Turn, Board, LastPit, SeedsInHand, NewBoard) :-
	step(Turn, Board, LastPit, SeedsInHand, NewBoard, NewPit, NewSeedsInHand).

% move a single step in a move - putting a seed from the player's
% hand into the next pit.
step(_, Board, LastPit, 0, Board, LastPit, 0) :- !.	
step(Turn, Board, LastPit, SeedsInHand, NewBoard, NewPit, NewSeedsInHand) :-
	Board = Player1Pits/Player2Pits,
	next_pit(Turn, LastPit, LastPit1),
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
	step(Turn, Board1, LastPit1, SeedsInHand1, NewBoard, NewPit, NewSeedsInHand).

% determine the next pit that a seed should go into
% will be the next player's pit, or his kalah after the last pit,
% or the first pit of the oponent's after the player's kalah
next_pit(Turn, LastPitPlayer/LastPitNo, NextPitPlayer/NextPitNo) :-
	pits(PitsPerPlayer),
	(
		Turn = LastPitPlayer, !, 
		MaxPitNo is PitsPerPlayer + 1
	;
		MaxPitNo is PitsPerPlayer
	),
	(
		LastPitNo is MaxPitNo, !, 
		next_player(LastPitPlayer, NextPitPlayer),
		NextPitNo = 1
	;
		NextPitPlayer = LastPitPlayer,
		NextPitNo is LastPitNo + 1
	)
	.

% will add a seed to the Nth pit in Pits, resulting in NewPits
add_seed_to_pit(Pits, N, NewPits) :-
	ArgNo is N + 2,    % first arg is functor, second is the player
	Pits =.. PitsList,
	change_list(PitsList, NewPitsList, ArgNo, add),
	NewPits =.. NewPitsList.

% change_list(L1, L2, N, add/empty).
% will copy a list of numbers from L1 to L2, 
% adding 1 to the Nth or zeroing it
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


% switching players. straightforward
next_player(player1, player2).
next_player(player2, player1).

%moves(Pos, PosList)
moves(Turn/0/P1Pits/P2Pits, PosList) :-
	
