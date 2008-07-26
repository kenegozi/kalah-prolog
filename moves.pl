/***********************************************************************
	Program	:	Kalah game in PROLOG
	Written by	:	Ken Egozi
	File		:	moves.pl
	Description	:	moving between states
***********************************************************************/

% 

move(Pos, NewPos) :-
	select_pit(Pos, MoveData, Pos1),
	move(Pos1, MoveData, NewPos).

%move(Pos, Player-PitNo/InHand, NewPos) :-

% if last step was in player's kalah - then he gets another round
move(Turn-Board, Turn-PitNo/0, NewPos) :-
	is_kalah(PitNo), !,
	move(Turn-Board, NewPos).

% if last step is a collect step - then collect
move(Turn-Board, Turn/PitNo/0, NewPos) :-
	next_player(Turn, Opposite),
	get_opposite_pit(PitNo, OppositePitNo),
	empty_pit(Opposite, Board, OppositePitNo, OppositeSeeds, Board1),
	(
		OppositeSeeds > 0, !,
		empty_pit(Turn, Board1, PitNo, Seeds, Board2),
		SeedsToAddToKalah is Seeds + OppositeSeeds,
		add_to_kalah(Turn, Board2, SeedsToAddToKalah, Board3),
		NewPos = Opposite-Board3
	;
		NewPos = Opposite-Board1
	).


get_opposite_pit(PitNo, OppositePitNo) :-
	is_kalah(KalahPitNo),
	OppositePitNo is KalahPitNo - PitNo.

is_empty_pit(player1, P1Pits/_, PitNo) :- !,
	ArgNo is PitNo + 1,
	arg(ArgNo, P1Pits, 0), !.

is_empty_pit(player2, _/P2Pits, PitNo) :- !,
	ArgNo is PitNo + 1,
	arg(ArgNo, P2Pits, 0), !.
 

empty_pit(player1, P1Pits/P2Pits, PitNo, Seeds, NewP1Pits/P2Pits) :-
	P1Pits =.. P1PitsList,
	NodeNo is PitNo + 2,
	pop_from_index(P1PitsList, NodeNo, NewP1PitsList, Seeds),
	NewP1Pits =.. NewP1PitsList.

move(Turn/P1Pits/P2Pits, Pit, SeedsInHand, NewPos) :-
	step(Turn/P1Pits/P2Pits, Pit, SeedsInHand, 0, Board, LastPit, 0).


select_pit(Turn/P1Pits/P2Pits, Pit/SeedsInHand, Turn/P1Pits1/P2Pits1).

% move a single step in a move - putting a seed from the player's
% hand into the next pit.
	
	% determine collect
	% determine ended_in_kalah->free_move
step(Turn/P1Pits/P2Pits, LastPit, SeedsInHand, Turn/NewP1Pits/NewP2Pits, NewPit, NewSeedsInHand) :-
	next_pit(Turn, LastPit, NewPitPlayer/NewPitNo),
	SeedsInHand1 is SeedsInHand - 1,
	(
		NewPitPlayer = player2,!,
		add_seed_to_pit(P2Pits, NewPitNo, NewP2Pits),
		Board1 = P1Pits/NewP2Pits
	;
		add_seed_to_pit(P1Pits, NewPitNo, NewP1Pits),
		Board1 = NewP1Pits/P2Pits
	),
	step(Turn/Board1, NewPitPlayer/NewPitNo, SeedsInHand1, Turn/NewP1Pits/NewP2Pits, NewPit, NewSeedsInHand).

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
%moves(Turn/P1Pits/P2Pits, PosList) :-

h(_/P1Pits/P2Pits, Val) :- 
	settings(pits_per_player(PitsPerPlayer)),
	KalahIndex is PitsPerPlayer + 2,
	arg(KalahIndex, P1Pits, P1Kalah),
	arg(KalahIndex, P2Pits, P2Kalah),!,
	P1Factor is 2^P1Kalah,
	P2Factor is 2^P2Kalah,
	Val is P1Factor - P2Factor.
	