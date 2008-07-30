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
	set_pits(2),!,
	Input = pits(player1,10,20,30),
	empty_pit(Input, 2, 20, Output),
	Output = pits(player1,10,0,30)
)).

:- end_setup_tests.
/***********************************************************************
% moves/put_seeds
***********************************************************************/
:- setup_tests('put_seeds/7').

:- test('enough for current player works and no seeds are left'/ ( 
	set_pits(6),!,
	Pits=pits(player1,1,1,1,1,1,1,1),
	put_seeds(player1, Pits, 1, 6, NewPits, LastPitNo, SeedsLeft),
	NewPits=pits(player1,2,2,2,2,2,2,1),
	LastPitNo=6,
	SeedsLeft=0
)).

:- test('enough for current kanah works and no seeds are left'/( 
	set_pits(6),!,
	Pits=pits(player1,1,1,1,1,1,1,1),
	put_seeds(player1, Pits, 2, 6, NewPits, LastPitNo, SeedsLeft),
	NewPits=pits(player1,1,2,2,2,2,2,2),
	LastPitNo=7,
	SeedsLeft=0
)).

:- test('enough for opponents kanah will skip the kanah'/( 
	set_pits(6),!,
	Pits=pits(player2,1,1,1,1,1,1,1),
	put_seeds(player1, Pits, 2, 6, NewPits, LastPitNo, SeedsLeft),
	NewPits=pits(player2,1,2,2,2,2,2,1),
	LastPitNo=6,
	SeedsLeft=1
)).

:- test('more than enough seeds works and seeds are left'/( 
	set_pits(6),!,
	Pits=pits(player1,1,1,1,1,1,1,1),
	put_seeds(player1, Pits, 4, 6, NewPits, LastPitNo, SeedsLeft),
	NewPits=pits(player1,1,1,1,2,2,2,2),
	LastPitNo=7,
	SeedsLeft=2
)).
:- end_setup_tests.

/***********************************************************************
% moves - step
***********************************************************************/
:- setup_tests('step/5').
:- test('sanity'/(
	set_pits(4),!,
	P1Pits=pits(player1,0,1,1,1,0),
	P2Pits=pits(player2,1,1,1,1,0),
	step(player1/P1Pits/P2Pits, player1/1, 3, player1/NewP1Pits/NewP2Pits, LastPlayerBoard/LastPitNo),
	NewP1Pits=pits(player1,0,2,2,2,0),
	NewP2Pits=pits(player2,1,1,1,1,0),
	LastPlayerBoard=player1,
	LastPitNo=4
)).
:- test('players kalah is used'/(
	set_pits(4),!,
	P1Pits=pits(player1,0,1,1,1,0),
	P2Pits=pits(player2,1,1,1,1,0),
	step(player1/P1Pits/P2Pits, player1/1, 4, player1/NewP1Pits/NewP2Pits, LastPlayerBoard/LastPitNo),
	NewP1Pits=pits(player1,0,2,2,2,1),
	NewP2Pits=pits(player2,1,1,1,1,0),
	LastPlayerBoard=player1,
	LastPitNo=5
)).
:- test('moving to opponents pits correctly'/(
	set_pits(4),!,
	P1Pits=pits(player1,0,1,1,1,0),
	P2Pits=pits(player2,1,1,1,1,0),
	step(player1/P1Pits/P2Pits, player1/1, 6, player1/NewP1Pits/NewP2Pits, LastPlayerBoard/LastPitNo),
	NewP1Pits=pits(player1,0,2,2,2,1),
	NewP2Pits=pits(player2,2,2,1,1,0),
	LastPlayerBoard=player2,
	LastPitNo=2
)).
:- test('opponents kalah is skipped'/(
	set_pits(4),!,
	P1Pits=pits(player1,0,1,1,1,0),
	P2Pits=pits(player2,1,1,1,1,0),
	step(player1/P1Pits/P2Pits, player1/1, 9, player1/NewP1Pits/NewP2Pits, LastPlayerBoard/LastPitNo),
	NewP1Pits=pits(player1,1,2,2,2,1),
	NewP2Pits=pits(player2,2,2,2,2,0),
	LastPlayerBoard=player1,
	LastPitNo=1
)).
:- end_setup_tests.

/***********************************************************************
% moves - move
***********************************************************************/

:- setup_tests('move').
:- test('several simple options'/(
	set_pits(3),!,
	bagof(P-N, move(player1/pits(player1,1,1,1,0)/pits(player2,1,1,1,0), P, N), Ps),
	Ps= [
		player2 / pits(player1,0,0,1,3) / pits(player2,1,0,1,0)-[1],
		player2 / pits(player1,1,0,0,3) / pits(player2,0,1,1,0)-[2],
		player2 / pits(player1,0,0,0,4) / pits(player2,1,0,1,0)-[3,1],
		player2 / pits(player1,1,0,0,3) / pits(player2,0,1,1,0)-[3,2]
	]
)).

:- test('cross to next'/(
	set_pits(2),!,
	bagof(P-N, move(player1/pits(player1,1,2,0)/pits(player2,1,1,0), P, N), Ps),
	Ps= [
		player2 / pits(player1,0,0,4) / pits(player2,0,1,0)-[1],
		player2 / pits(player1,1,0,1) / pits(player2,2,1,0)-[2]
	]
)).

:- test('will collect when opposite is not empty'/(
	set_pits(2),!,
	bagof(P-N, move(player1/pits(player1,1,0,0)/pits(player2,1,0,0), P, N), Ps),
	Ps= [
		player2 / pits(player1,0,0,2) / pits(player2,0,0,0)-[1]
	]
)).

:- test('will not collect when opposite is empty'/(
	set_pits(2),!,
	bagof(P-N, move(player1/pits(player1,1,0,0)/pits(player2,0,1,0), P, N), Ps),
	Ps= [
		player2 / pits(player1,0,1,0) / pits(player2,0,1,0)-[1]
	]
)).

:- test('will not collect when end in opponent pit'/(
	set_pits(2),!,
	bagof(P-N, move(player1/pits(player1,1,3,0)/pits(player2,0,0,0), P, N), Ps),
	Ps= [
		player2 / pits(player1,0,4,0) / pits(player2,0,0,0)-[1],
		player2 / pits(player1,1,0,1) / pits(player2,1,1,0)-[2]
	]
)).
:- end_setup_tests.


/***********************************************************************
% moves - opposite_pit/2
***********************************************************************/
:- setup_tests('opposite_pit/2').

:- test('for odd no. of pits => works'/(
	set_pits(3),!,
	opposite_pit(1, 3)
)).

:- test('for even no. of pits => works'/(
	set_pits(4),!,
	opposite_pit(2, 3)
)).

:- end_setup_tests.


/***********************************************************************
	select_pit(Pits, Number/Seeds, NewPits).
***********************************************************************/
:- setup_tests('select_pit/3').
:- test('when all has values -> selects all'/(
	set_pits(2),!,
	bagof(N/S/P, select_pit(pits(player1,2,4,0), N/S, P), Selected),
	Selected=[1 / 2 / pits(player1,0,4,0),2 / 4 / pits(player1,2,0,0)]
)).
:- test('when one has value -> selects only that one'/(
	set_pits(2),!,
	bagof(N/S/P, select_pit(pits(player1,0,3,0), N/S, P), Selected),
	Selected=[2 / 3 / pits(player1,0,0,0)]
)).
:- test('when none has value -> fail'/(
	set_pits(2),!,
	not bagof(N/S/P, select_pit(pits(player1,0,0,0), N/S, P), _)
)).
:- end_setup_tests.

/***********************************************************************
% setup helpers
***********************************************************************/
	
set_pits :-
	set_pits(6). 
set_pits(P) :-
	(retract(pits(_)) ; true),
	assert(pits(P)).
