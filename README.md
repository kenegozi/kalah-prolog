kalah-prolog
============

PROLOG implementation for the kalah game. Created as a project for "PROLOG - aspects for AI" of the Israeli Open University 


Rules:

Kalah is played on a board of two rows, each consisting of six round pits that have a 
large store at either end called kalah. 

A player owns the six pits closest to him and the Kalah on his right side. 
Beginners may start less pits (with less seeds in each pit), but the game becomes more and 
more challenging as the number of pits rise.

In the game we allow up to eight pits, to make things interesting.

Play is counterclockwise. 
The seeds are distributed one by one in the pits and the players own kalah, 
but not into the opponents store.

* If the last seed is dropped into an opponents pit or a non-empty pit of the player, 
  the move ends without anything being captured.
  
* If the last seed falls into the players kalah, he must move again.

* If the last seed is put into an empty pit owned by the player, and the opposite pit is not empty
  he captures all contents of the opposite pit together with the capturing piece and puts them in his kalah. 
  
If in a player's turn there is no legal move (all his puts are empty), he looses the turn and
the opponent continue to have another turn.

The game ends when all of the pits are emptied.
The player with more seeds in his kalah, wins.
