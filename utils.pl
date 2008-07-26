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


	
/**********************************************************************/
