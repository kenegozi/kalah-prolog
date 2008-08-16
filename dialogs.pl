/***********************************************************************
	Program	:	Kalah game in PROLOG
	Written by	:	Ken Egozi
	File		:	dialogs.pl
	Description	:	dialog windows declerations - to be used with
                        the Dialog Editor
***********************************************************************/



/*The entry window for the game*/
dlg_main_window :- 
   _S1 = [ws_caption,ws_sysmenu,dlg_ownedbyprolog],
   _S2 = [ws_child,ws_visible,ss_center],
   _S3 = [ws_child,ws_tabstop,ws_visible,bs_pushbutton,bs_text,bs_center,bs_vcenter],
   wdcreate(  dlg_main_window,        `Kalah`,              160, 45, 356, 176, _S1 ),
   wccreate( (dlg_main_window,11000), static, `Kalah Game`,  30, 40, 290,  40, _S2 ),
   wccreate( (dlg_main_window,1000),  button, `New Game`,    30, 90, 130,  30, _S3 ),
   wccreate( (dlg_main_window,1001),  button, `Exit`,       190, 90, 130,  30, _S3 ).

/*Options dialog - when starting a new game*/
dlg_options :- 
   _S1 = [ws_caption,dlg_ownedbyprolog,ws_ex_topmost,ws_ex_dlgmodalframe],
   _S3 = [ws_child,ws_visible,ws_ex_right,ss_left],
   _S4 = [ws_child,ws_tabstop,ws_visible,cbs_dropdownlist,cbs_autohscroll,cbs_disablenoscroll,ws_vscroll],
   _S5 = [ws_child,ws_visible,ws_tabstop,cbs_dropdownlist,cbs_autohscroll,cbs_disablenoscroll,ws_vscroll],
   _S6 = [ws_child,ws_tabstop,ws_visible,bs_pushbutton,bs_text,bs_center,bs_vcenter],
   wdcreate(  dlg_options,        `Options`,                   160,  45, 366, 256, _S1 ),
   wccreate( (dlg_options,11001), static,   `Board size:`,      60,  70,  80,  20, _S3 ),
   wccreate( (dlg_options,11002), static,   `Level:`,           60, 120,  80,  20, _S3 ),
   wccreate( (dlg_options,5000),  combobox, `BoardSize`,        150,  70,  40,  80, _S4 ),
   wccreate( (dlg_options,5001),  combobox, `Level`,            150, 120, 100,  80, _S5 ),
   wccreate( (dlg_options,1000),  button,   `Start game`,       60,  160, 130,  30, _S6 ),
   wccreate( (dlg_options,1001),  button,   `Cancel`,           60,  190, 130,  30, _S6 ).
on_show(dlg_options):-
	set_combo_values((dlg_options,5000), 6, [4,5,6,7,8]),
	set_combo_values((dlg_options,5001), `Regular`, [`Easy`,`Regular`,`Expert`]).


/*Actual gameboard window*/
dlg_game_board :- 
	_S1 = [ws_sysmenu,ws_popup,ws_caption,dlg_ownedbyprolog],
	_S2 = [ws_child,ws_border,ws_visible],
	_S3 = [ws_child,ws_visible,ws_tabstop,bs_pushbutton],
	_S4 = [ws_child,ws_tabstop,ws_visible,bs_pushbutton,bs_text,bs_center,bs_vcenter],
 
	pits(Size),
	board_size(Size, BoardWidth, BoardHeight),
	WindowHeight is BoardHeight + 100,
	WindowWidth is BoardWidth + 40,
	wdcreate(  dlg_game_board,        `Kalah`,      160,  45, WindowWidth , WindowHeight , _S1 ),
	wccreate( (dlg_game_board,10000), grafix, `Board`,  20, 40,  BoardWidth, BoardHeight, _S2 ),
	wccreate( (dlg_game_board,10002), button, `End Game`,  50, 10, 70 , 20, _S4 ).

game_board( (dlg_game_board,10000) ).
