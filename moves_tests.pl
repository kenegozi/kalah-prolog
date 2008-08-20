/***********************************************************************
	Program	:	Kalah game in PROLOG
	Written by	:	Ken Egozi
	File		:	test_fixtures.pl
	Description	:	unit tests for moves.pl
***********************************************************************/

test_in_moves_module([
	test__empty_pit,
	test__put_seeds,
	test__step,
	test__move,
	test__opposite_pit,
	test__select_pit
]).

/***********************************************************************
empty_pit(Player, InputPits, PitNo, SeedsThatWereInSaidPit, OutputPits),
***********************************************************************/
test__empty_pit__works:-
	set_pits(2),!,
	Input = pits(player1,10,20,30),
	empty_pit(Input, 2, 20, Output),
	Output = pits(player1,10,0,30).

test__empty_pit(`empty_pit`,[
	test__empty_pit__works
]).


/***********************************************************************
moves/put_seeds
***********************************************************************/

test__put_seeds__enough_for_current_player_works_and_no_seeds_are_left:-
	set_pits(6),!,
	Pits=pits(player1,1,1,1,1,1,1,1),
	put_seeds(player1, Pits, 1, 6, NewPits, LastPitNo, SeedsLeft),
	NewPits=pits(player1,2,2,2,2,2,2,1),
	LastPitNo=6,
	SeedsLeft=0.

test__put_seeds__enough_for_current_kanah_works_and_no_seeds_are_left:-
	set_pits(6),!,
	Pits=pits(player1,1,1,1,1,1,1,1),
	put_seeds(player1, Pits, 2, 6, NewPits, LastPitNo, SeedsLeft),
	NewPits=pits(player1,1,2,2,2,2,2,2),
	LastPitNo=7,
	SeedsLeft=0.

test__put_seeds__enough_for_opponents_kanah_will_skip_the_kanah:-
	set_pits(6),!,
	Pits=pits(player2,1,1,1,1,1,1,1),
	put_seeds(player1, Pits, 2, 6, NewPits, LastPitNo, SeedsLeft),
	NewPits=pits(player2,1,2,2,2,2,2,1),
	LastPitNo=6,
	SeedsLeft=1.

test__put_seeds__more_than_enough_seeds_works_and_seeds_are_left:-
	set_pits(6),!,
	Pits=pits(player1,1,1,1,1,1,1,1),
	put_seeds(player1, Pits, 4, 6, NewPits, LastPitNo, SeedsLeft),
	NewPits=pits(player1,1,1,1,2,2,2,2),
	LastPitNo=7,
	SeedsLeft=2.

test__put_seeds(`put_seeds`,[
	test__put_seeds__enough_for_current_player_works_and_no_seeds_are_left,
	test__put_seeds__enough_for_current_kanah_works_and_no_seeds_are_left,
	test__put_seeds__enough_for_opponents_kanah_will_skip_the_kanah,
	test__put_seeds__more_than_enough_seeds_works_and_seeds_are_left
]).

/***********************************************************************
moves - step
***********************************************************************/
test__step__sanity:-
	set_pits(4),!,
	P1Pits=pits(player1,0,1,1,1,0),
	P2Pits=pits(player2,1,1,1,1,0),
	step(player1/P1Pits/P2Pits, player1/1, 3, player1/NewP1Pits/NewP2Pits, LastPlayerBoard/LastPitNo),
	NewP1Pits=pits(player1,0,2,2,2,0),
	NewP2Pits=pits(player2,1,1,1,1,0),
	LastPlayerBoard=player1,
	LastPitNo=4.

test__step__players_kalah_is_used:-
	set_pits(4),!,
	P1Pits=pits(player1,0,1,1,1,0),
	P2Pits=pits(player2,1,1,1,1,0),
	step(player1/P1Pits/P2Pits, player1/1, 4, player1/NewP1Pits/NewP2Pits, LastPlayerBoard/LastPitNo),
	NewP1Pits=pits(player1,0,2,2,2,1),
	NewP2Pits=pits(player2,1,1,1,1,0),
	LastPlayerBoard=player1,
	LastPitNo=5.

test__step__moving_to_opponents_pits_correctly:-
	set_pits(4),!,
	P1Pits=pits(player1,0,1,1,1,0),
	P2Pits=pits(player2,1,1,1,1,0),
	step(player1/P1Pits/P2Pits, player1/1, 6, player1/NewP1Pits/NewP2Pits, LastPlayerBoard/LastPitNo),
	NewP1Pits=pits(player1,0,2,2,2,1),
	NewP2Pits=pits(player2,2,2,1,1,0),
	LastPlayerBoard=player2,
	LastPitNo=2.

test__step__opponents_kalah_is_skipped:-
	set_pits(4),!,
	P1Pits=pits(player1,0,1,1,1,0),
	P2Pits=pits(player2,1,1,1,1,0),
	step(player1/P1Pits/P2Pits, player1/1, 9, player1/NewP1Pits/NewP2Pits, LastPlayerBoard/LastPitNo),
	NewP1Pits=pits(player1,1,2,2,2,1),
	NewP2Pits=pits(player2,2,2,2,2,0),
	LastPlayerBoard=player1,
	LastPitNo=1.

test__step(`step`,[
	test__step__sanity,
	test__step__players_kalah_is_used,
	test__step__moving_to_opponents_pits_correctly,
	test__step__opponents_kalah_is_skipped
]).

/***********************************************************************
moves - move
***********************************************************************/
test__move__several_simple_options:-
	set_pits(3),!,
	bagof(P, move(player1/pits(player1,1,1,1,0)/pits(player2,1,1,1,0), P), Ps),
	Ps= [
		player2 / pits(player1,0,0,1,3) / pits(player2,1,0,1,0) - 1 / 1 / special(player1 / 2 / 2 / 2 / 1),
		player2 / pits(player1,1,0,0,3) / pits(player2,0,1,1,0) - 2 / 1 / special(player1 / 3 / 2 / 1 / 1),
		player1 / pits(player1,1,1,0,1) / pits(player2,1,1,1,0) - 3 / 1 / kalah(player1)
	].

test__move__cross_to_next:-
	set_pits(2),!,
	bagof(P, move(player1/pits(player1,1,2,0)/pits(player2,1,1,0), P), Ps),
	Ps= [
		player2 / pits(player1,0,0,4) / pits(player2,0,1,0) - 1 / 1 / special(player1 / 2 / 3 / 1 / 1),
		player2 / pits(player1,1,0,1) / pits(player2,2,1,0) - 2 / 2 / _
	].

test__move__will_collect_when_opposite_is_not_empty:-
	set_pits(2),!,
	bagof(P, move(player1/pits(player1,1,0,0)/pits(player2,1,0,0), P), Ps),
	Ps= [
		player2 / pits(player1,0,0,2) / pits(player2,0,0,0) - 1 / 1 / special(player1 / 2 / 1 / 1 / 1)
	].

test__move__will_not_collect_when_opposite_is_empty:-
	set_pits(2),!,
	bagof(P, move(player1/pits(player1,1,0,0)/pits(player2,0,1,0), P), Ps),
	Ps= [
		player2 / pits(player1,0,1,0) / pits(player2,0,1,0) - 1 / 1 / _
	].

test__move__will_not_collect_when_end_in_opponent_pit:-
	set_pits(2),!,
	bagof(P, move(player1/pits(player1,1,3,0)/pits(player2,0,0,0), P), Ps),
	Ps= [
		player2 / pits(player1,0,4,0) / pits(player2,0,0,0) - 1 / 1 / _,
		player2 / pits(player1,1,0,1) / pits(player2,1,1,0) - 2 / 3 / _
	].

test__move__when_move_will_result_in_empty_pits_and_ends_in_kalah_will_find_the_move:-
	set_pits(2),!,
	bagof(P, move(player2/pits(player1,1,1,1)/pits(player2,0,1,0), P), Ps),
	Ps=	[
		player1 / pits(player1,1,1,1) / pits(player2,0,0,1) - 2 / 1 / kalah(player2)
].

test__move(`move`,[
	test__move__several_simple_options,
	test__move__cross_to_next,
	test__move__will_collect_when_opposite_is_not_empty,
	test__move__will_not_collect_when_opposite_is_empty,
	test__move__will_not_collect_when_end_in_opponent_pit,
	test__move__when_move_will_result_in_empty_pits_and_ends_in_kalah_will_find_the_move
]).


/***********************************************************************
opposite_pit/2
***********************************************************************/
test__opposite_pit__for_odd_no_of_pits_works:-
	set_pits(3),!,
	opposite_pit(1, 3).

test__opposite_pit__for_even_no_of_pits_works:-
	set_pits(4),!,
	opposite_pit(2, 3).

test__opposite_pit(`opposite_pit`,[
	test__opposite_pit__for_odd_no_of_pits_works,
	test__opposite_pit__for_even_no_of_pits_works
]).

/***********************************************************************
	select_pit(Pits, Number/Seeds, NewPits).
***********************************************************************/
test__select_pit__when_all_has_values_selects_all:-
	set_pits(2),!,
	bagof(N/S/P, select_pit(pits(player1,2,4,0), N/S, P), Selected),
	Selected=[1 / 2 / pits(player1,0,4,0),2 / 4 / pits(player1,2,0,0)].

test__select_pit__when_one_has_value_selects_only_that_one:-
	set_pits(2),!,
	bagof(N/S/P, select_pit(pits(player1,0,3,0), N/S, P), Selected),
	Selected=[2 / 3 / pits(player1,0,0,0)].

test__select_pit__when_none_has_value_fail:-
	set_pits(2),!,
	not bagof(N/S/P, select_pit(pits(player1,0,0,0), N/S, P), _).

test__select_pit(`select_pit`,[
	test__select_pit__when_all_has_values_selects_all,
	test__select_pit__when_one_has_value_selects_only_that_one,
	test__select_pit__when_none_has_value_fail
]).

/***********************************************************************
setup helpers
***********************************************************************/
	
set_pits :-
	set_pits(6). 
