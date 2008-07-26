/***********************************************************************
	Program	:	Kalah game in PROLOG
	Written by	:	Ken Egozi
	File		:	test_fixtures.pl
	Description	:	unit tests
***********************************************************************/




/***********************************************************************
% utils/is_in_range
***********************************************************************/
tests(utils/is_in_range, [
	is_in_range__when_given_correct_input__satisfied,
	is_in_range__when_equal_to_min__satisfied,
	is_in_range__when_equal_to_max__satisfied,
	is_in_range__when_given_out_of_range_input__dissatisfied
]).

is_in_range__when_given_correct_input__satisfied :-
	is_in_range(2, 1-3).

is_in_range__when_equal_to_min__satisfied :-
	is_in_range(1, 1-2).

is_in_range__when_equal_to_max__satisfied :-
	is_in_range(2, 1-2).

is_in_range__when_given_out_of_range_input__dissatisfied :-
	not is_in_range(4, 1-3).


/***********************************************************************
% utils/in_range
***********************************************************************/
tests(utils/in_range, [
	in_range__when_given_correct_input__generates,
	in_range__when_given_incorrect_input__fails
]).

in_range__when_given_correct_input__generates :-
	bagof(X, in_range(X, 1-3), Xs),
	Xs = [1,2,3].

in_range__when_given_incorrect_input__fails :-
	not bagof(X, in_range(X, 3-1), _).
	

/***********************************************************************
% utils/create_list
***********************************************************************/
tests(utils/create_list, [
	create_list__always__creates_the_list_with_the_correct_data,
	create_list__always__creates_the_list_with_the_correct_length
]).

create_list__always__creates_the_list_with_the_correct_data :-
	create_list(List, 3, 2),
	assert_all_members_equal_to(List, 2).

create_list__always__creates_the_list_with_the_correct_length :-
	create_list(List, 3, 2),
	length(List, 3).
	

/***********************************************************************
% moves/put_seeds
***********************************************************************/
/*
tests(moves/put_seeds, [
	set_pits,
	put_seeds__enough_for_current_player__works_and_no_seeds_are_left,
	put_seeds__enough_for_current_kanah__works_and_no_seeds_are_left,
	put_seeds__more_than_enough_seeds__works_and_seeds_are_left
]).
*/
test_moves(setup, (
	assert(a)
)).
test_moves(teardown, (
	retract(a)
)).
test_moves(put_seeds/enough_for_current_player__works_and_no_seeds_are_left, ( 
	a
%	Pits=pits(player1,1,1,1,1,1,1,1),
%	put_seeds(Pits, 1, 6, NewPits, SeedsLeft),
%	NewPits=pits(player1,2,2,2,2,2,2,1),
%	SeedsLeft=0
)).

test_moves(put_seeds/enough_for_current_kanah__works_and_no_seeds_are_left, ( 
a
%	Pits=pits(player1,1,1,1,1,1,1,1),
%	put_seeds(Pits, 2, 6, NewPits, SeedsLeft),
%	NewPits=pits(player1,1,2,2,2,2,2,2),
%	SeedsLeft=0
)).

test_moves(put_seeds/more_than_enough_seeds__works_and_seeds_are_left, ( 
a
%	Pits=pits(player1,1,1,1,1,1,1,1),
%	put_seeds(Pits, 4, 6, NewPits, SeedsLeft),
%	NewPits=pits(player1,1,1,1,2,2,2,2),
%	SeedsLeft=2
)).

put_seeds(pits(player1,1,1,1,1,1,1,1), _, _, pits(player1,2,2,2,2,2,2,1), 0).

test_moves :-
	(test_moves(setup, Setup),!;Setup
	bagof((Predicate/Name,Test), test_moves(Predicate/Name,Test), [T|Ts]),
	foreach([T|Ts]).

foreach([]):-!.
foreach([(Predicate/Name,Test)|Ts]):-
	write('testing '), write(Predicate), write('/'),write(Name),write('...'),
	assert((run_test:-Test)),
	(run_test,!, write('OK') ; write('FAIL')),
	nl,
	retract((run_test:-Test)),
	foreach(Ts).

/***********************************************************************
% moves/get_opposite_pit
***********************************************************************/
tests(moves/get_opposite_pit, [
	get_opposite_pit__for_odd_pits__works,
	get_opposite_pit__for_even_pits__works
]).

get_opposite_pit__for_odd_pits__works :-
	set_pits(3),
	get_opposite_pit(1, 3).

get_opposite_pit__for_even_pits__works :-
	set_pits(4),
	get_opposite_pit(2, 3).

/***********************************************************************
% moves/empty_pit
***********************************************************************/
tests(moves/empty_pit, [
	empty_pit__works
]).

empty_pit__works :-
	Input = pits(player1,10,20,30)/null,
	empty_pit(player1, Input, 2, 20, Output),
	Output = pits(player1,10,0,30)/null.

/***********************************************************************
% utils/conc
***********************************************************************/
tests(utils/conc, [
	conc__empty_and_empty__return_empty,
	conc__empty_and_nonempty__return_L2,
	conc__nonempty_and_empty__return_L1,
	conc__nonempty_and_nonempty__return_L1concL2
]).

conc__empty_and_empty__return_empty :-
	conc([], [], []).

conc__empty_and_nonempty__return_L2 :-
	conc([], [1,2], [1,2]).

conc__nonempty_and_empty__return_L1 :-
	conc([1,2], [], [1,2]).

conc__nonempty_and_nonempty__return_L1concL2 :-
	conc([1,2], [3,4], [1,2,3,4]).




/***********************************************************************
% moves/change_list
***********************************************************************/
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



/***********************************************************************
% moves/move
***********************************************************************/
tests(moves/move, [
	set_pits,
	move__when_ends_within_same_player_pits__works
]).

move__when_ends_within_same_player_pits__works :-
	P1P=pits(player1,3,3,3,0,0,0,0),
	P2P=pits(player2,3,3,3,0,0,0,0),
	Last=player1/1,
	move(player1,P1P/P2P,Last,4,N).


/***********************************************************************
% moves/step
***********************************************************************/
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


/***********************************************************************
% moves/next_pit
***********************************************************************/
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



/***********************************************************************
% setup helpers
***********************************************************************/
	
set_pits :-
	set_pits(6).
set_pits(P) :-
	(retract(pits(_)) ; true),
	assert(pits(P)).
