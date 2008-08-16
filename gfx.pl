
%writing a number at a given position
write_text(P, Left, Top, Right, Bottom):-
	number(P),
	P < 10,!,
	to_text(P, Text),
	HMiddle is (Right - Left) / 2 + Left - 8,
	VMiddle is (Bottom - Top) / 2 + Top - 8,
	gfx((font=font_pits->
		text(HMiddle, VMiddle, Text))).
write_text(P, Left, Top, Right, Bottom):-
	number(P),
	A is P mod 10,
	B is P // 10,
	ALeft is Left + 4,
	ARight is Right + 4,	
	BLeft is Left - 4,
	BRight is Right - 4,	
	write_text(A, ALeft, Top, ARight, Bottom),
	write_text(B, BLeft, Top, BRight, Bottom).




% gfx_text - creating a string object in a given font out of text
gfx_text( Font, String, X, Y ) :-
   (  gfx_device( Device )
   -> (  type( String, 4 ),
         type( X, 0 ),
         type( Y, 0 )
      -> gfx_font_handle( Font, Handle ),
         len( String, Length ),
         wintxt( [], -1, -1, Text ),
         wintxt( [], 8, 0, `` ),
         gfxdev( [Device], 0, Shell, Index ),
         winapi( (gdi32,'SelectObject'), [Device,Handle], 0, _ ),
         winapi( (gdi32,'GetTextExtentPointA'), [Device,String,Length,[]], 0, _ ),
         gfxdev( [Device], 0, Shell, Index ),
         wintxt( [], 8, 0, Data ),
         wintxt( [], -1, -1, Text ),
         (  getx( 4, X ),
            getx( 4, Y )
         ) <~ Data
      ;  type( String, 0 )
      -> throw( 22, gfx_text(Font,String,X,Y) )
      ;  throw( 23, gfx_text(Font,String,X,Y) )
      )
   ;  throw( 11, gfx_text(Font,String,X,Y) )
   ).

% back door to find the device context - please don't worry how it works!
:- abolish( gfx_device/1 ),
   dict( 1, Dictionary ),
   member( Predicate, Dictionary ),
   cmp( 0, Predicate, known_device ),
   !,
   assert( (gfx_device(Device):-Predicate(_,_,Device,_)) ).




