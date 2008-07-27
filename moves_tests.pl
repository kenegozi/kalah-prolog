/***********************************************************************
	Program	:	Kalah game in PROLOG
	Written by	:	Ken Egozi
	File		:	test_fixtures.pl
	Description	:	unit tests for moves.pl
***********************************************************************/

:- ensure_loaded('unit_tests').


/***********************************************************************
	empty_pit(Player, InputPits, PitNo, SeedsThatWereInSaidPit, OutputPits),
***********************************************************************/
:- setup_tests('empty_pit/5').

:- test('works'/(
	Input = pits(player1,10,20,30)/null,
	empty_pit(player1, Input, 2, 20, Output),
	Output = pits(player1,10,0,30)/null
)).

:- end_setup_tests.

/***********************************************************************
% moves/change_list
***********************************************************************/

/*

tests(moves/change_list, [
	change_list__add_first__works,
	change_list__add_middle__works,
	change_list__add_last__works,
	change_list__empty_first__works,
	change_list__add_middle__works,
	change_list__add_last__works
]).

change_list__add_first__works :-
	L = [1,1,1],
	change_list(L, L1, 1, add),
	L1 = [2,1,1].

change_list__add_middle__works :-
	L = [1,1,1],
	change_list(L, L1, 2, add),
	L1 = [1,2,1].

change_list__add_last__works :-
	L = [1,1,1],
	change_list(L, L1, 3, add),
	L1 = [1,1,2].

change_list__empty_first__works :-
	L = [1,1,1],
	change_list(L, L1, 1, empty),
	L1 = [0,1,1].

change_list__add_middle__works :-
	L = [1,1,1],
	change_list(L, L1, 2, empty),
	L1 = [1,0,1].

change_list__add_last__works :-
	L = [1,1,1],
	change_list(L, L1, 3, empty),
	L1 = [1,1,0].


*/

/***********************************************************************
% moves/move
***********************************************************************/

/*
tests(moves/move, [
	set_pits,
	move__when_ends_within_same_player_pits__works
]).

move__when_ends_within_same_player_pits__works :-
	P1P=pits(player1,3,3,3,0,0,0,0),
	P2P=pits(player2,3,3,3,0,0,0,0),
	Last=player1/1,
	move(player1,P1P/P2P,Last,4,N).

*/
/***********************************************************************
% moves/step
***********************************************************************/

/*
tests(moves/step, [
	set_pits(3),
	step__when_ends_within_same_player_pits__works,
	step__when_ends_within_next_player_pits__works,
	step__when_ends_back_in_player_pits__works
]).

step__when_ends_within_same_player_pits__works :-
	P1P=pits(player1,0,0,0,0),
	P2P=pits(player2,0,0,0,0),
	step(player1, P1P/P2P, player1/1, 1, NewP1P/P2P, _, _),
	NewP1P=pits(player1,0,1,0,0).
	
step__when_ends_within_next_player_pits__works :-
	P1P=pits(player1,0,0,0,0),
	P2P=pits(player2,0,0,0,0),
	step(player1, P1P/P2P, player1/1, 4, NewP1P/NewP2P, _, _),
	NewP1P=pits(player1,0,1,1,1),
	NewP2P=pits(player2,1,0,0,0).

step__when_ends_back_in_player_pits__works :-
	P1P=pits(player1,0,0,0,0),
	P2P=pits(player2,0,0,0,0),
	step(player1, P1P/P2P, player1/1, 8, NewP1P/NewP2P, _, _),
	NewP1P=pits(player1,2,1,1,1),
	NewP2P=pits(player2,1,1,1,0).

*/
/***********************************************************************
% moves/next_pit
***********************************************************************/

/*
tests(moves/next_pit, [
	set_pits(3),
	next_pit__when_in_players_first_pit__moves_to_next,
	next_pit__when_in_players_last_pit__moves_to_kalah,
	next_pit__when_in_players_kalah__move_to_opponents_first,
	next_pit__when_in_opponents_last_pit__moves_to_players_first
]).

next_pit__when_in_players_first_pit__moves_to_next :-
	next_pit(player1, player1/1, player1/2).

next_pit__when_in_players_last_pit__moves_to_kalah :-
	next_pit(player1, player1/3, player1/4).

next_pit__when_in_players_kalah__move_to_opponents_first :-
	next_pit(player1, player1/4, player2/1).

next_pit__when_in_opponents_last_pit__moves_to_players_first :-
	next_pit(player1, player2/3, player1/1).


*/
/***********************************************************************
% moves/put_seeds
***********************************************************************/
:- setup_tests('put_seeds(Pits, StartPitNo, SeedsInHand, NewPits, SeedsLeft)').

:- test('enough for current player works and no seeds are left'/ ( 
	Pits=pits(player1,1,1,1,1,1,1,1),
	put_seeds(Pits, 1, 6, NewPits, SeedsLeft),
	NewPits=pits(player1,2,2,2,2,2,2,1),
	SeedsLeft=0
)).

:- test('enough for current kanah works and no seeds are left'/( 
	Pits=pits(player1,1,1,1,1,1,1,1),
	put_seeds(Pits, 2, 6, NewPits, SeedsLeft),
	NewPits=pits(player1,1,2,2,2,2,2,2),
	SeedsLeft=0
)).

:- test('more than enough seeds works and seeds are left'/( 
	Pits=pits(player1,1,1,1,1,1,1,1),
	put_seeds(Pits, 4, 6, NewPits, SeedsLeft),
	NewPits=pits(player1,1,1,1,2,2,2,2),
	SeedsLeft=2
)).

:- end_setup_tests.

put_seeds(pits(player1,1,1,1,1,1,1,1), _, _, pits(player1,2,2,2,2,2,2,1), 0).
	

/***********************************************************************
% moves - get_opposite_pit/2
***********************************************************************/
:- setup_tests('get_opposite_pit/2').

:- test('for odd no. of pits => works'/(
	set_pits(3),
	get_opposite_pit(1, 3)
)).

:- test('for even no. of pits => works'/(
	set_pits(4),
	get_opposite_pit(2, 3)
)).

:- end_setup_tests.


/***********************************************************************
	select_pit(Pits, Number/Seeds, NewPits).
***********************************************************************/
:- setup_tests('select_pit/3').

:- test('when all has values -> selects all'/(
	
	select_pit(pits(player1,1,2,0), N/S, P),
)).

:- test
N = S = 2 ,
P = pits(player1,1,0,0)

| ?- select_pit(pits(player1,0,2,0), N/S, P).
N

/***********************************************************************
% setup helpers
***********************************************************************/
	
set_pits :-
	set_pits(6). 
set_pits(P) :-
	(retract(pits(_)) ; true),
	assert(pits(P)).

