/***********************************************************************
	Program		:	Kalah game in PROLOG
	Written by	:	Ken Egozi
	File		:	utils.pl
	Description	:	various useful predicates
***********************************************************************/


% Usage: conc(L1,L2,L1_concatenated_with_L2)
conc([], L, L) :- !.
conc([H|T1], L2, [H|T2]) :-
	conc(T1, L2, T2).

% Usage: in_range(THE_INPUT, MINVALUE-MAXVALUE)
% a different manner
is_in_range(X, LBound-UBound) :-
  X >= LBound, 
  X =< UBound.

% in_range(LBound, X, UBound)
% The predicate is non deterministic so do not use it in
in_range(X, X-X) :- !.
in_range(X, X-UBound) :- 
	X < UBound.
in_range(X, LBound-UBound) :-
	LBound =< UBound,
	NewLBound is LBound + 1,
	in_range(X, NewLBound-UBound).


% Useful to initialise a list with repeating values
% Usage: create_list(LIST_TO_CREATE, LENGTH, VALUE_TO_PUT_IN_EACH_NODE)
create_list([], 0, _) :- !.
create_list([InitialValue | Tail], Length, InitialValue) :-
	Length1 is Length - 1,
	create_list(Tail, Length1, InitialValue).



% pop_from_index(List, Index, NewListWith_0_AtIndex, ValueThatWasAtIndex)
% pop_from_index([1,2,3], 2, [1,0,3], 2).
pop_from_index([H|T], 1, [0|T], H) :- !.
pop_from_index([H|T], Index, [H|T1], Val) :- 
	NewIndex is Index - 1,
	pop_from_index(T, NewIndex, T1, Val).




% copy_list_and_add(List, Skip, ToAdd, NewList)
copy_list_and_add(L, _, 0, L) :- !.
copy_list_and_add([H|T], Skip, ToAdd, [H|T1]) :-
	Skip > 0, !, 
	Skip1 is Skip - 1,
	copy_list_and_add(T, Skip1, ToAdd, T1).
copy_list_and_add([H|T], 0, ToAdd, [H1|T1]) :-
	H1 is H + 1,
	ToAdd1 is ToAdd - 1,
	copy_list_and_add(T, 0, ToAdd1, T1).


%add_to_last(L1, N, L2).
% L2 is L1, but the last node is incremented by N
add_to_last([H], N, [H1]):-
	integer(H),!,
	H1 is H + N .

add_to_last([H|T], N, [H|T1]):-
	add_to_last(T, N, T1).

	
/**********************************************************************/

% sleeping the current thread - useful for letting the GUI finish work at some
% cases
sleep(0):-!.
sleep(N):-N<0,!.
sleep(N):-
	N1 is N - 0.004,
	sleep(N1).
