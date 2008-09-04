/***********************************************************************
	Program	:	Kalah game in PROLOG
	Written by	:	Ken Egozi
	File		:	unit_tests.pl
	Description	:	unit tests runner
	Usage		:	simply call run_tests/0
***********************************************************************/

all_tests([
	test_in_moves_module,
	tests_in_utils_module
]).


run_tests :- 
	clear_test_results,
	all_tests(AllModules),
	run_modules_tests(AllModules),
	print_test_results.

run_modules_tests([]) :- !.
run_modules_tests([Module|T]) :-
	Module(PredicatesToTests),
	write('Testing module: '), write(Module), nl,
	run_predicate_tests(PredicatesToTests),
	run_modules_tests(T).

run_predicate_tests([]) :- !.
run_predicate_tests([PredicateToTest|T]) :-
	PredicateToTest(Name, Tests),
	write('Testing predicate: '), write(PredicateToTest), nl,
	run_tests(Name, Tests),
	run_predicate_tests(T).

run_tests(_,[]) :- !.
run_tests(Name, [Test|T]) :-
	write('Running test: '), write(Test), write(' ... '),
	(Test,!,
		write('Passed'), nl,
		test_result_passed(Name, Test)
	;
		write('Failed'), nl,
		test_result_failed(Name, Test)
	),
	run_tests(Name, T).

test_result_passed(Name, _):-
	(retract(test_result(Name, Passed, FailedList)),!,
		Passed1 is Passed + 1,
		FailedList1=FailedList
	;
		Passed1=1,
		FailedList1=[]
	),
	assert(test_result(Name, Passed1, FailedList1)).

test_result_failed(Name, Test):-
	(retract(test_result(Name, Passed, FailedList)),!,
		Passed1=Passed,
		FailedList1=[Test|FailedList]
	;
		Passed1=0,
		FailedList1=[Test]
	),
	assert(test_result(Name, Passed1, FailedList1)).


clear_test_results:-
	retractall(test_result(_, _, _)).


print_test_results:-
	bagof(N/P/Fs, test_result(N,P,Fs), Results),
	test_results_sum(Results, Passed, Failed),
	nl,
	write('total passed: '), write(Passed), nl,
	len(Failed, F),
	(F > 0,!,
		write('total failed: '), write(F), nl,
		print_failed_Tests(Failed)
	;
		write('all green :)'),nl
	).

print_failed_Tests([]):-!.
print_failed_Tests([F|T]):-
	write('  '), write(F), nl.


test_results_sum([], 0, []):-!.
test_results_sum(Results, Passed, Failed):-
	test_results_sum(Results, 0, [], Passed, Failed).

test_results_sum([], Passed, Failed, Passed, Failed):-!.
test_results_sum([_/P/Fs|Tail], PassedUntilNow, FailedUntilNow, Passed, Failed):-
	Passed1 is PassedUntilNow  + P,
	conc(Fs, FailedUntilNow, Failed1),
	test_results_sum(Tail, Passed1, Failed1, Passed, Failed).



% Asserts
assert_all_members_equal_to([], _).
assert_all_members_equal_to([H|T], H) :-
	assert_all_members_equal_to(T, H).


