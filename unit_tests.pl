/***********************************************************************
	Program	:	Kalah game in PROLOG
	Written by	:	Ken Egozi
	File		:	unit_tests.pl
	Description	:	unit tests runner
	Usage		:	simply call run_tests/0
***********************************************************************/

run_tests  :-
	dynamic([
		tests_passed/1,
		failing_tests/1,
		total_tests_passed/1,
		total_failing_tests/1 ]),
	assert(tests_passed(0)),
	assert(failing_tests([])),
	assert(total_tests_passed(0)),
	assert(total_failing_tests([])),
	bagof(
		(Module/Predicate, Tests), 
		tests(Module/Predicate, Tests), 
		TestDefinitions),
	run_tests_definitions(TestDefinitions),
	retract(total_tests_passed(TotalPassedAtEnd)),		
	retract(total_failing_tests(TotalFailedAtEnd)),
	len(TotalFailedAtEnd, TotalFailedAtEndCount),
	write('summary:'), nl,
	write('Passed: '), write(TotalPassedAtEnd), 
	write(' Failed: '), write(TotalFailedAtEndCount), nl, nl,
	(TotalFailedAtEndCount> 0, write_fails(TotalFailedAtEnd) ; write('Alles Gut'), nl).

run_tests_definitions([]) :- !.
run_tests_definitions([(Module/Predicate, Tests)|T]) :-
	write('module: '), write(Module), 
	write(' predicate: '), write(Predicate),
	write(' ... '),
	run_tests(Tests),
	retract(tests_passed(Passed)),		
	retract(failing_tests(Failed)),
	assert(tests_passed(0)),
	assert(failing_tests([])),
	len(Failed, FailedCount),
	write('Passed: '), write(Passed), 
	write(' Failed: '), write(FailedCount), nl, 
	retract(total_tests_passed(TotalPassed)),		
	retract(total_failing_tests(TotalFailed)),
	NewTotalPassed is TotalPassed + Passed,
	conc(Failed, TotalFailed, NewTotalFailed),
	assert(total_tests_passed(NewTotalPassed)),		
	assert(total_failing_tests(NewTotalFailed)),
	run_tests_definitions(T).


write_fails([]) :- !.
write_fails([H|T]) :- 
	write_fails(T),
	write(H), write(' failed'), nl.

run_tests([]) :- !.
run_tests([H|T]) :-
	run_test(H),
	run_tests(T).

run_test(Test) :-
	call(Test),!,
	tests_passed(X),
	retract(tests_passed(X)), 
	NewX is X + 1,
	assert(tests_passed(NewX)).

run_test(Test) :-
	failing_tests(X),
	retract(failing_tests(X)),
	NewX = [Test|X],
	assert(failing_tests(NewX)).


% Asserts
assert_all_members_equal_to([], _).
assert_all_members_equal_to([H|T], H) :-
	assert_all_members_equal_to(T, H).


