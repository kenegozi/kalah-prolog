/***********************************************************************
	Program	:	Kalah game in PROLOG
	Written by	:	Ken Egozi
	File		:	dialog_handler.pl
	Description	:	event handler for dialogs
                        dealing with:
				   msg_paint
                           msg_button
                           msg_mousemove, (X,Y)
                        forwarding any uncatched event to Windows
***********************************************************************/

/*
%redirect to dialog_handler/2 for specified resultless and dataless handlers
dialog_handler(W,M,_,_) :-

	dialog_handler(W,M).

%redirect to dialog_handler/3 for specified dataless handlers
dialog_handler(W,M,D,_) :-
	dialog_handler(W,M,D).
*/


/*********************************************************************
dlg_main_window
**********************************************************************/

% 'New game' button click
dialog_handler((dlg_main_window,1000),msg_button,_,_) :- !,
	show_window(dlg_options),
	wclose(dlg_main_window).

% 'Exit' button click
dialog_handler((dlg_main_window,1001),msg_button,_,_) :- !,
	wclose(dlg_main_window).
dialog_handler(dlg_main_window,msg_close,_,_) :- !,
	wclose(dlg_main_window).


/*********************************************************************
dlg_options
**********************************************************************/
	
% 'Start game' button click
dialog_handler((dlg_options,1000),msg_button,_,_) :- !,
	wtext( (dlg_options,5000), Selected ), 
	to_text(Size, Selected),
	set_pits(Size),
	show_window(dlg_game_board),
	wtext( (dlg_options,5001), Level ), 
	set_level(Level),
	wtext( (dlg_options,5002), First ), 
	set_first(First),
	wclose(dlg_options),
	assert_pit_map,
	sleep(1000),
	start_new_game.

% 'Cancel' button click
dialog_handler((dlg_options,1001),msg_button,_,_) :- !,
	show_window(dlg_main_window),
	wclose(dlg_options). 
dialog_handler(dlg_options,msg_close,_,_) :- !,
	show_window(dlg_main_window),
	wclose(dlg_options). 





/*********************************************************************
dlg_game_board
**********************************************************************/

% 'End Game' button click
dialog_handler((dlg_game_board,10002),msg_button,_,_) :- !,
	show_window(dlg_main_window),
	wclose(dlg_game_board). 

dialog_handler((dlg_game_board,10001),msg_button,_,_) :- !,
	start_new_game.

dialog_handler(dlg_game_board,msg_close,_,_) :- !,
	show_window(dlg_main_window),
	wclose(dlg_game_board). 


dialog_handler( (dlg_game_board,10000), msg_paint, _, _) :-!,
	draw_board_bg,
	draw_all_pits,
	show_messages.

dialog_handler( (dlg_game_board,10000), msg_leftdown, (X, Y), _) :-!,
	game_state(waiting),
	turn(player1),
	get_pit(X/Y, PitNo),
	set_game_state(player1_playing),
	player1_move(PitNo),
	show_state_message,
	draw_all_pits,
	set_game_state(waiting),
	play.

/*********************************************************************
catch all
**********************************************************************/
% forward any uncatched message to Windows API	
dialog_handler(Window,Message,Data,Result) :- 
/**
	write('W:'),write(Window),nl,
	write('M:'),write(Message),nl,
	write('D:'),write(Data),nl,
	write('R:'),write(Result),nl,
	write('----------'),nl,
/**/
   window_handler(Window,Message,Data,Result). 
