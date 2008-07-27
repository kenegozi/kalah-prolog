/***********************************************************************
	Program	:	Kalah game in PROLOG
	Written by	:	Ken Egozi
	File		:	unit_tests.pl
	Description	:	unit tests runner
	Usage		:	simply call run_tests/0
***********************************************************************/

test(Fact/Test):-
	current_predicate_under_test(Predicate),
	retractall(test_def(Predicate/Fact/Test)),
	assert(test_def(Predicate/Fact/Test)).

setup_tests(Predicate) :-
	retractall(test_def(Predicate/_/_)),
	assert(current_predicate_under_test(Predicate)).

end_setup_tests:-
	retractall(current_predicate_under_test(_)).

run_tests :-
	dynamic(tests_stats/2),
	bagof(P/Tests, bagof((Fact/Test), test_def(P/Fact/Test), Tests), TestsPerPredicate),
	run_tests(TestsPerPredicate, Passed/Failed),
	write_tests_summary(Passed/Failed).

run_tests(TestsTestsPerPredicate, TotalPassed/TotalFailed) :-
	run_tests(TestsTestsPerPredicate, 0/0, TotalPassed/TotalFailed).

run_tests([], Passed/Failed, Passed/Failed):-!.

run_tests([P/Tests|Rest], Passed/Failed, TotalPassed/TotalFailed):-
	write('testing '), write(P), 
	foreach_test(Tests, PassedInPredicate/FailedInPredicate),
	write(' passed:'), write(PassedInPredicate),
	(FailedInPredicate > 0, write(' failed:'), write(FailedInPredicate) ; true),
	nl,
	Passed1 is Passed + PassedInPredicate,
	Failed1 is Failed + FailedInPredicate,
	run_tests(Rest, Passed1/Failed1, TotalPassed/TotalFailed).

foreach_test(Tests, Passed/Failed):-
	foreach_test(Tests, 0/0, Passed/Failed).

foreach_test([], Passed/Failed, Passed/Failed):-!.

foreach_test([Fact/Test|Rest], Passed/Failed, NewPassed/NewFailed):-
	assert((run_test:-Test)),
	(
		run_test, !,
		NextPassed is Passed + 1,
		NextFailed is Failed
	;
		NextFailed is Failed + 1,
		NextPassed is Passed,
		write('FAIL: '), write(Fact), nl
	),
	retract((run_test:-Test)),
	foreach_test(Rest, NextPassed/NextFailed, NewPassed/NewFailed).


write_tests_summary(Passed/0) :- !,
	nl,
	write(Passed), write(' tests passed :)'),
	nl.

write_tests_summary(Passed/Failed) :-
	nl,
	write(Passed), write(' tests passed, however'), nl,
	write(Failed), write(' tests failed :('),
	nl.

reset_all_tests:-
	retractall(test_def(_/_/_)).



% Asserts
assert_all_members_equal_to([], _).
assert_all_members_equal_to([H|T], H) :-
	assert_all_members_equal_to(T, H).


