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
	move__when_ends_within_same_player_pits__works
]).

move__when_ends_within_same_player_pits__works :-
	P1P=pits(player1,3,3,3,0),
	P2P=pits(player2,3,3,3,0),
	Last=player1/1,
	move(P1P/P2P,Last,4,N).


/***********************************************************************
% moves/step
***********************************************************************/
tests(moves/step, [
	step__when_ends_within_same_player_pits__works,
	step__when_ends_within_next_player_pits__works
]).

step__when_ends_within_same_player_pits__works :-
	P1P=pits(player1,0,0,0,0),
	P2P=pits(player2,0,0,0,0),
	step(P1P/P2P, player1/1, 1, NewP1P/P2P, _, _),
	NewP1P=pits(player1,0,1,0,0).
	
step__when_ends_within_next_player_pits__works :-
	P1P=pits(player1,0,0,0,0),
	P2P=pits(player2,0,0,0,0),
	step(P1P/P2P, player1/1, 4, NewP1P/NewP2P, _, _),
	NewP1P=pits(player1,0,1,1,1),
	NewP2P=pits(player2,1,0,0,0).
