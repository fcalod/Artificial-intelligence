
% Guarda qué fichas quedan
% TODO: Cómo manejamos el orden de las entradas?
rem_pieces( (0, 0) ).
rem_pieces( (0, 1) ).
rem_pieces( (0, 2) ).
rem_pieces( (0, 3) ).
rem_pieces( (0, 4) ).
rem_pieces( (0, 5) ).
rem_pieces( (0, 6) ).
rem_pieces( (1, 1) ).
rem_pieces( (1, 2) ).
rem_pieces( (1, 3) ).
rem_pieces( (1, 4) ).
rem_pieces( (1, 5) ).
rem_pieces( (1, 6) ).
rem_pieces( (2, 2) ).
rem_pieces( (2, 3) ).
rem_pieces( (2, 4) ).
rem_pieces( (2, 5) ).
rem_pieces( (2, 6) ).
rem_pieces( (3, 3) ).
rem_pieces( (3, 4) ).
rem_pieces( (3, 5) ).
rem_pieces( (3, 6) ).
rem_pieces( (4, 4) ).
rem_pieces( (4, 5) ).
rem_pieces( (4, 6) ).
rem_pieces( (5, 5) ).
rem_pieces( (5, 6) ).
rem_pieces( (6, 6) ).

% Guarda los valores disponibles de las dos fichas junto a las que puede colocarse una nueva
% Inicia en un estado inválido, antes de que se coloquen fichas
game_state(-1, -1).

% TODO: ordenar una ficha de menor entrada a mayor antes de utilizarla / quitarla de la mesa
% TODO: hacer que el sistema confirme la entrada o permita deshacer la jugada, en caso de que nos equivoquemos al ingresarla
% Side: "L" (Left) o "R" (R)
place_piece([N1, N2], Side):-
  retract(rem_pieces( (N1, N2) )), % quita la ficha del pool disponible
  .



