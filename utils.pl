/***********************************************************************
	Program		:	Kalah game in PROLOG

	Written by	:	Ken Egozi
	
	File		:	utils.pl

	Description	:	various useful predicates
***********************************************************************/


conc([], L, L) :- !.
conc([H|T1], L2, [H|T2]) :-
	conc(T1, L2, T2).

% Usage: in_range(THE_INPUT, MINVALUE-MAXVALUE)
% The predicate is non deterministic so do not use it in
% a different manner
in_range(X, Min-Max) :-
  X >= Min, 
  X =< Max.

% Useful to initialise a list with repeating values
% Usage: create_list(LIST_TO_CREATE, LENGTH, VALUE_TO_PUT_IN_EACH_NODE)
create_list([], 0, _) :- !.
create_list([InitialValue | Tail], Length, InitialValue) :-
	Length1 is Length - 1,
	create_list(Tail, Length1, InitialValue).


	
/**********************************************************************/
