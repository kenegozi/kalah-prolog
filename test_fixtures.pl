/***********************************************************************
	Program	:	Kalah game in PROLOG

	Written by	:	Ken Egozi
	
	File		:	test_fixtures.pl

	Description	:	unit tests
***********************************************************************/


/***********************************************************************
% utils.pl
***********************************************************************/

tests(utils/in_range, [
	in_range__when_given_correct_input__satisfied,
	in_range__when_equal_to_min__satisfied,
	in_range__when_equal_to_max__satisfied,
	in_range__when_given_out_of_range_input__dissatisfied
]).

in_range__when_given_correct_input__satisfied :-
	in_range(2, 1-3).

in_range__when_equal_to_min__satisfied :-
	in_range(1, 1-2).

in_range__when_equal_to_max__satisfied :-
	in_range(2, 1-2).

in_range__when_given_out_of_range_input__dissatisfied :-
	not in_range(4, 1-3).


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
% moves.pl
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
% moves.pl - move
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
% moves.pl - step
***********************************************************************/

tests(moves/step, [
	step__when_ends_within_same_player_pits__works,
	step__when_ends_within_next_player_pits__works
]).

step__when_ends_within_same_player_pits__works :-
	P1P=pits(player1,0,0,0,0),
	P2P=pits(player2,0,0,0,0),
	Last=player1/1,
	step(P1P/P2P, Last, 0, NewP1P/P2P, _, _),
	NewP1P=pits(player1,0,1,0,0).
	
step__when_ends_within_next_player_pits__works :-
	P1P=pits(player1,0,0,0,0),
	P2P=pits(player2,0,0,0,0),
	Last=player1/1,
	step(P1P/P2P, Last, 4, NewP1P/NewP2P, _, _),
	NewP1P=pits(player1,0,1,1,1),
	NewP2P=pits(player2,1,0,0,0).

