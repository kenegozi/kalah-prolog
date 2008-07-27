/***********************************************************************
	Program	:	Kalah game in PROLOG
	Written by	:	Ken Egozi
	File		:	utils_tests.pl
	Description	:	unit tests for utils.pl
***********************************************************************/
:- ensure_loaded('unit_tests').


/***********************************************************************
	is_in_range(X,Min-Max)
***********************************************************************/
:- setup_tests('is_in_range/2').
:- test('when given correct input -> satisfied'/(
	is_in_range(2, 1-3)
)).


:- test('when equal to min -> satisfied '/(
	is_in_range(1, 1-2)
)).

:- test('when equal to max -> satisfied'/(
	is_in_range(2, 1-2)
)).

:- test('when given out of range input -> fails'/(
	not is_in_range(2, 1-3)
)).
:- end_setup_tests.


/***********************************************************************
	in_range(X/Min-Max)
***********************************************************************/
:- setup_tests('in_range/2').
:- test('when given correct input -> generates'/(
	bagof(X, in_range(X, 1-3), Xs),
	Xs = [1,2,3]
)).

:- test('when given incorrect input -> fails'/(
	not bagof(X, in_range(X, 3-1), _)
)).
:- end_setup_tests.
	

/***********************************************************************
	create_list(List, Length, Value)
***********************************************************************/
:- setup_tests('create_list/3').

:- test('always creates the list with the correct values'/(
	create_list(List, 3, 2),
	assert_all_members_equal_to(List, 2)
)).

:- test('always creates the list with the correct length'/(
	create_list(List, 3, 2),
	length(List, 3)
)).
	
:- end_setup_tests.

/***********************************************************************
	conc/3
***********************************************************************/
:- setup_tests('conc/3'). 
:- test('empty and empty returns empty'/(
conc([], [], [])
)). 
:- test('empty and nonempty returns L2'/(
conc([], [1,2], [1,2])
)). 
:- test('nonempty and empty returns L1'/(
conc([1,2], [], [1,2])
)). 
:- test('nonempty and nonempty returns L1 concatenated with L2'/(
conc([1,2], [3,4], [1,2,3,4])
)). 
:- end_setup_tests.
