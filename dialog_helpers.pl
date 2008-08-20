/***********************************************************************
	Program	:	Kalah game in PROLOG
	Written by	:	Ken Egozi
	File		:	dialog_helpers.pl
	Description	:	various useful facts and predicates for 
                        helping with the dialogs and handlers syntax
***********************************************************************/


% quick map from a digit to a string representation
to_text(1, ` 1`):-!.
to_text(2, ` 2`):-!.
to_text(3, ` 3`):-!.
to_text(4, ` 4`):-!.
to_text(5, ` 5`):-!.
to_text(6, ` 6`):-!.
to_text(7, ` 7`):-!.
to_text(8, ` 8`):-!.
to_text(9, ` 9`):-!.
to_text(0, ` 0`):-!.


% quick map from player name to player atom
to_player(`Human`, player1):-!.
to_player(`Computer`, player2):-!.

% quick map from level to depth of search in tree
level(1, `Easy`):-!.
level(3, `Regular`):-!.
level(6, `Expert`):-!.
depth(D):-
	level(L),!,
	level(D,L).


% messages
message(player1_kalah,   `Kalah - you have another turn :)`).
message(player1_collect, `You have just collected seeds from the computer :)`).
message(player2_kalah,   `Computer's Kalah - he has have another turn :(`).
message(player2_collect, `The computer has just collected seeds from you :(`).
message(player1_no_move, `No moves left - turn over to computer :(`).
message(player2_no_move, `No moves left - turn over to you :)`).

show_state_message:-
	game_state(State),
	message(State, Message),!,
	add_message(Message).
show_state_message.


%create a dialog, show it, and wireup to a handler, raising events during
show_window(W):-
	W,
	(on_show(W),! ; true ),
	show_dialog(W),
	window_handler(W, dialog_handler).


% will set a list of values in a combo, pre-selecting one of the values
set_combo_values(Combo, Selected, Values) :-
	set_combo_values(Combo, Selected, Values, 0).

set_combo_values(Combo, Selected, [], _) :- !,
	(to_text(Selected, Text),! ; Selected=Text),
	wcmbfnd(Combo, -1, Text, Pos ), 
	wcmbsel(Combo, Pos, 1 ).

set_combo_values(Combo, Selected, [Value|Vs], Pos) :-
	(to_text(Value, Text),! ; Value=Text),
	wcmbadd(Combo, -1, Text, Pos ),
	NextPos is Pos + 1,
	set_combo_values(Combo, Selected, Vs, NextPos).


