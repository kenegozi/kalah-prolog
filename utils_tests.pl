/***********************************************************************
	Program	:	Kalah game in PROLOG
	Written by	:	Ken Egozi
	File		:	utils_tests.pl
	Description	:	unit tests for utils.pl
***********************************************************************/

tests_in_utils_module([
	test__is_in_range,
	test__in_range,
	test__create_list,
	test__conc,
	test__copy_list_and_add
]).

/***********************************************************************
	is_in_range(X,Min-Max)
***********************************************************************/
test__is_in_range__when_given_correct_input__satisfied:-
	is_in_range(2, 1-3).

test__is_in_range__when_equal_to_min__satisfied:-
	is_in_range(1, 1-2).

test__is_in_range__when_equal_to_max__satisfied:-
	is_in_range(2, 1-2).

test__is_in_range__when_given_out_of_range_input__fails:-
	not is_in_range(4, 1-3).

test__is_in_range(`is_in_range`,[
	test__is_in_range__when_given_correct_input__satisfied,
	test__is_in_range__when_equal_to_min__satisfied,
	test__is_in_range__when_equal_to_max__satisfied,
	test__is_in_range__when_given_out_of_range_input__fails
]).

/***********************************************************************
	in_range(X/Min-Max)
***********************************************************************/
test__in_range__when_given_correct_input__generates:-
	bagof(X, in_range(X, 1-3), Xs),
	Xs = [1,2,3].

test__in_range__when_given_incorrect_input__fails:-
	not bagof(X, in_range(X, 3-1), _).

test__in_range(`in_range`,[
	test__in_range__when_given_correct_input__generates,
	test__in_range__when_given_incorrect_input__fails
]).

/***********************************************************************
	create_list(List, Length, Value)
***********************************************************************/
test__create_list__always_creates_the_list_with_the_correct_values:-
	create_list(List, 3, 2),
	assert_all_members_equal_to(List, 2).

test__create_list__always_creates_the_list_with_the_correct_length:-
	create_list(List, 3, 2),
	length(List, 3).

test__create_list(`create_list`,[
	test__create_list__always_creates_the_list_with_the_correct_values,
	test__create_list__always_creates_the_list_with_the_correct_length
]).

/***********************************************************************
	conc/3
***********************************************************************/
test__conc__empty_and_empty_returns_empty:-
	conc([], [], []).

test__conc__empty_and_nonempty_returns_L2:-
	conc([], [1,2], [1,2]).

test__conc___nonempty_and_empty_returns_L1:-
	conc([1,2], [], [1,2]).

test__conc__nonempty_and_nonempty_returns_L1_concatenated_with_L2:-
	conc([1,2], [3,4], [1,2,3,4]).

test__conc(`conc`,[
	test__conc__empty_and_empty_returns_empty,
	test__conc__empty_and_nonempty_returns_L2,
	test__conc___nonempty_and_empty_returns_L1,
	test__conc__nonempty_and_nonempty_returns_L1_concatenated_with_L2
]).

/***********************************************************************
	copy_list_and_add(List, Skip, ToAdd, NewList) conc/3
***********************************************************************/
test__copy_list_and_add__can_process_part_of_the_list:-
	setof(Skip/ToAdd, (MaxToAdd/X/L/L_AsFunctor)^(
		in_range(Skip, 0-3),
		MaxToAdd is 4 - Skip,
		in_range(ToAdd, 0-MaxToAdd),
		copy_list_and_add([0,0,0,0], Skip, ToAdd, L),
		L_AsFunctor =.. [list|L],
		in_range(X, 1-4),
		( X =< Skip,
			arg(X, L_AsFunctor, 0)
		;
			arg(X, L_AsFunctor, 1)
	)), Options),
	Options = [0/1,0/2,0/3,0/4,1/0,1/1,1/2,1/3,2/0,2/1,2/2,3/0,3/1].

test__copy_list_and_add(`copy_list_and_add`,[
	test__copy_list_and_add__can_process_part_of_the_list
]).

