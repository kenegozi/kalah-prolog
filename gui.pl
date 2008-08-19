/***********************************************************************
	Program	:	Kalah game in PROLOG
	Written by	:	Ken Egozi
	File		:	gui.pl
	Description	:	gui related facts and predicates to interact 
                        with the user and control application flow
***********************************************************************/


start :-
	set_brushes,
	show_window(dlg_main_window).


/**************************************
Visual settings
***************************************/
basic_width(30).
vertical_separator(10).
horizontal_separator(20).
padding(20).
default_pit_colour(lightbrown).
default_bg_colour(brown).

get_board_settings(BasicWidth/VSep/HSep/Padding) :-
	basic_width(BasicWidth),
	vertical_separator(VSep),
	horizontal_separator(HSep),
	padding(Padding).



set_brushes :-
	gfx_brush_create(brush_blue,0,0,255,solid), 
	gfx_brush_create(brush_lightbrown,190,181,174,solid), 
	gfx_brush_create(brush_brown,124,106,93,solid), 
	gfx_brush_create(brush_red,255,0,0,solid), 
	gfx_font_create( font_pits, 'Arial', 17, normal ).


% mapping colours to brushes
get_brush(brown, brush_brown).
get_brush(lightbrown, brush_lightbrown).
get_brush(blue, brush_blue).
get_brush(red, brush_red).


/**************************************
Data calculations
***************************************/

%calculate the needed board size for the game size
board_size(Size, W, H) :-
	basic_width(BasicWidth),
	vertical_separator(VerticalSeparator),
	horizontal_separator(HorizontalSeparator),
	padding(Padding),
	W is 
		(Size + 2) * BasicWidth + 
		(Size + 1) * VerticalSeparator +
		2          * Padding,
	H is
		2 * 	BasicWidth + 
			HorizontalSeparator +
		2          * Padding.

% dimensions of a pit
pit_size(pit, Width,Width) :-
	basic_width(Width).

% dimensions of a kalah
pit_size(kalah, Width,Height) :-
	basic_width(Width),
	horizontal_separator(Separator),
	Height is Width * 2 + Separator.

/**************************
Game board background
***************************/
draw_board_bg:-
	game_board(Board),
	default_bg_colour(Colour),
	get_brush(Colour, Brush),
	pits(Size),
	board_size(Size, BoardWidth, BoardHeight),
	gfx_begin(Board),
	gfx((brush=Brush->
		rectangle(0, 0, BoardWidth, BoardHeight)
		)),
	gfx_end(Board).

/**********************************
Draw the pits
***********************************/

% draw a pit with a given colour at a given anchor
draw_pit(P, Left/Top, BasicWidth, Colour) :-
	Bottom is Top + BasicWidth,
	Right is Left + BasicWidth,
	draw_pit(P, Left, Top, Right, Bottom, Colour).

% draw a pit with a given colour at a given location
draw_pit(P, Left, Top, Right, Bottom, Colour) :-
	get_brush(Colour, Brush),
	gfx((brush=Brush->
		ellipse(Left, Top, Right, Bottom)
		)),
	write_text(P, Left, Top, Right, Bottom).

%drawing a player one pit (lower row)
draw_pit(P, player1, PitNo, BasicWidth/VSep/HSep/Padding, Colour) :-
	Top is Padding + BasicWidth + HSep,
	Left is Padding + (BasicWidth + VSep) * PitNo,
	draw_pit(P, Left/Top, BasicWidth , Colour).

%drawing a player two pit (upper row)
draw_pit(P, player2, PitNo, BasicWidth/VSep/HSep/Padding, Colour) :-
	Top is Padding,
	Left is Padding + (BasicWidth + VSep) * PitNo,
	draw_pit(P, Left/Top, BasicWidth , Colour).

%drawing the player one kalah (lower row)
draw_kalah(P, player1, PitNo, BasicWidth/VSep/HSep/Padding, Colour) :-
	Top is Padding,
	Bottom is Top + BasicWidth * 2 + HSep,
	Left is Padding + (BasicWidth + VSep) * PitNo,
	Right is Left + BasicWidth,
	draw_pit(P, Left, Top, Right, Bottom, Colour).

%drawing the player two kalah (upper row)
draw_kalah(P, player2, _, BasicWidth/_/HSep/Padding, Colour) :-
	Top is Padding,
	Bottom is Top + BasicWidth * 2 + HSep,
	Left is Padding,
	Right is Left + BasicWidth,
	draw_pit(P, Left, Top, Right, Bottom, Colour).

draw_all_pits:-
	pos(_/P1/P2),!,
	draw_all_pits(P1,P2).

%call to drawing all pits
draw_all_pits(P1,P2) :- !,
	game_board(Board),
	gfx_begin(Board),
	draw_pits(P1),
	draw_pits(P2),
	gfx_end(Board).	

%call to drawing a particular player's pits
draw_pits(Pits) :-
	get_board_settings(Settings),
	default_pit_colour(Colour),
	Pits =.. [pits, Player|PitsList],
	draw_pits(Player, PitsList, 1, Settings, Colour).

draw_pits(Player, [P], PitNo, Settings, Colour) :- !,
	draw_kalah(P, Player, PitNo, Settings, Colour).
draw_pits(Player, [P|Ps], PitNo, Settings, Colour) :-
	(Player=player1,!,
		PitToDraw=PitNo
	;
		pits(Size),
		PitToDraw is Size - PitNo + 1
	),
	draw_pit(P, Player, PitToDraw, Settings, Colour),
	NextPitNo is PitNo + 1,
	draw_pits(Player, Ps, NextPitNo, Settings, Colour).


% set memory
set_pits(P) :-
	(retract(pits(_)) ; true),!,
	assert(pits(P)).

set_level(L) :-
	(retract(level(_)) ; true),!,
	assert(level(L)).

set_first(F) :-
	(retract(first(_)) ; true),!,
	assert(first(F)).


assert_pos :-
	pits(P),
	create_list(P1PitsList, P, P),
	conc(P1PitsList, [0], P1PitsListWithKalah),
	P1Pits =.. [pits,player1|P1PitsListWithKalah],
	create_list(P2PitsList, P, P),
	conc(P2PitsList, [0], P2PitsListWithKalah),
	P2Pits =.. [pits,player2|P2PitsListWithKalah],
	first(First),
	to_player(First, Player),
	set_pos(Player/P1Pits/P2Pits).

set_pos(Pos) :-
	(retract(pos(_)); true),
	assert(pos(Pos)).


assert_pit_map :-
	(retract(map_player1_pit(_,_)); true),
	get_board_settings(Settings),
	pits(Size),
	assert_pit_map(Size, Settings).

assert_pit_map(0, _) :-!.
assert_pit_map(PitNo, BasicWidth/VSep/HSep/Padding) :-
	Top is Padding + BasicWidth + HSep,
	Left is Padding + (BasicWidth + VSep) * PitNo,
	Bottom is Top + BasicWidth * 2 + HSep,
	Right is Left + BasicWidth,
	Margin=5,
	Top1 is Top + Margin,
	Left1 is Left  + Margin,
	Bottom1 is Bottom - Margin,
	Right1 is Right - Margin,
	assert(map_player1_pit(PitNo, Top1/Left1/Bottom1/Right1)),
	NextPit is PitNo - 1,
	assert_pit_map(NextPit, BasicWidth/VSep/HSep/Padding).

in_pit(X/Y, PitNo):-
	map_player1_pit(PitNo, Top/Left/Bottom/Right),
	Y >= Top, Y =< Bottom,
	X >= Left, X =<   Right.

pit(Pit) :-
	pits(Size),
	in_range(Pit, 1-Size).

get_pit(X/Y, PitNo) :-
	pit(Pit),
	in_pit(X/Y, Pit),
	Pit=PitNo.

	


draw_pit(P, player1, PitNo, BasicWidth/VSep/HSep/Padding, Colour) :-
	Top is Padding + BasicWidth + HSep,
	Left is Padding + (BasicWidth + VSep) * PitNo,
	draw_pit(P, Left/Top, BasicWidth , Colour).
		
start_new_game :-
	set_game_state(new),
	assert_pos,
	draw_board_bg,
	draw_all_pits,
	play.

set_game_state(S):-
	(retract(game_state(_)) ; true),
	assert(game_state(S)).

show_game_over_message:-
	winner(Player),
	player_win_message(Player, Message),
	msgbox('Game over', Message, 0, _).

player_win_message(player1, 'You won - the computer is useless').
player_win_message(player2, 'The computer has won - good luk next time').
player_win_message(tie, 'Tie - try again').
