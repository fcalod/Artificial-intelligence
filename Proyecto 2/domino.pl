%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%   Estado de juego   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% main inicializa el estado del juego
% 
% game_state guarda todo lo que se conoce acerca del estado actual: fichas que no se han jugado, número de fichas por jugador, 
% número de fichas en el pozo, fichas que tenemos nosotros y las dos fichas abiertas en el tablero
:- dynamic game_state/1, game_state/2, turn/1, next_turn/0.
:- use_module(library(clpfd)).
main:-
  % Guarda las fichas del oponente y el pozo
  assert(game_state(rem_tiles, [%(0, 0), (0, 1), (0, 2), (0, 3), (0, 4), (0, 5), (0, 6), 
                                (1, 1), (1, 2), (1, 3), (1, 4), (1, 5), (1, 6), (2, 2), 
                                (2, 3), (2, 4), (2, 5), (2, 6), (3, 3), (3, 4), (3, 5), 
                                (3, 6), (4, 4), (4, 5), (4, 6), (5, 5), (5, 6), (6, 6)])),
  assert(game_state(player_hand, [(0, 0), (0, 1), (0, 2), (0, 3), (0, 4), (0, 5), (0, 6)])),
  assert(game_state(open_tiles, [])), % las dos fichas junto a las que se puede colocar una nueva
  assert(game_state(num_player_tiles, 7)), % inicia en 0 para contarlas después
  assert(game_state(num_oppo_tiles, 7)),
  assert(game_state(stock_size, 14)), % fichas en el pozo (inicialmente incluye las del jugador)
  assert(game_state(oppo_missing, [])), % fichas que el oponente no tiene
  assert(game_state(oppo_passed, 0)), % true si el oponente pasó el turno pasado
  assert(game_state(player_passed, 0)), % true si el jugador pasó el turno pasado
  assert(minimax_depth(10)), % profundidad a la que se explora el árbol minimax
  read_ini_player_hand,
  set_starting_player,
  first_turn,
  game_loop.

% ------------------------- Flujo del juego -------------------------
% Lee y guarda las fichas que tiene el jugador
read_ini_player_hand:-
 	game_state(num_player_tiles, Num_player_tiles), % cuántas fichas llevamos
	% Se detiene cuando llegamos a 7 fichas
	(	Num_player_tiles < 7 ->
		(
		( Num_player_tiles = 0 ->
			% Si es la primera ficha
			writeln("Ingresa cada ficha con el formato 'N1, N2.' y oprime enter"), !;
			% En caso contrario
			write("Tienes "), write(Num_player_tiles), write(" fichas en tu mano. Ingresa otra"), nl
		),
		
		read(Tile),
		order_input(Tile, Ordered_tile),
		
		(
			% Confirma la entrada
			reenter_input(Ordered_tile, read_ini_player_hand), !;
			% Revisa la ficha ingresada
			tile_in_stock_or_oppo(Ordered_tile, read_ini_player_hand), !;
			
			% Actualiza el estado del juego (sólo llega a este punto si la entrada es correcta)
			add_player_tile(Ordered_tile),
			% Lee la siguiente entrada
			read_ini_player_hand
		)
		); ! % regresa verdadero una vez que acaba
  ).

% Establece qué jugador empieza
set_starting_player:-
	writeln("Escribe '1.' si empezamos nosotros o '0.' si empieza el oponente"),
	read(Turn),
	assert(turn(Turn)).

% Coloca la primera ficha
first_turn:-
	write("¿Qué ficha se jugó?"), nl,
	read((Open_tile_N1, Open_tile_N2)), % haremos que la ficha tirada se convierte en la abierta
	order_input((Open_tile_N1, Open_tile_N2), (Ordered_open_N1, Ordered_open_N2)),
	(
		reenter_input((Ordered_open_N1, Ordered_open_N2), first_turn);
		tile_exists((Ordered_open_N1, Ordered_open_N2), first_turn);
		
		retract(game_state(open_tiles, [])),
		assert(game_state(open_tiles, [Ordered_open_N1, Ordered_open_N1])), % truco para usar place_tile
		place_tile((Ordered_open_N1, Ordered_open_N2), Ordered_open_N1)
	).

% Lleva a cabo todos los turnos (excepto el primero) hasta que acaba el juego
game_loop:-
	print_game_state,
  next_turn,
  turn(Turn),
  ( Turn = 1 ->
  		% Decide a quién le toca
  		player_turn, !;
   		oppo_turn
  ),
  game_state(num_player_tiles, Num_player_tiles),
  game_state(num_oppo_tiles, Num_oppo_tiles),
  game_state(oppo_passed, Oppo_passed),
  game_state(player_passed, Player_passed),
  (	(Num_player_tiles = 0; Num_oppo_tiles = 0; (Oppo_passed = 1, Player_passed = 1)) ->
			% El juego acaba si alguno no tiene fichas o los dos pasaron de forma consecutiva
			end_game, !;
			game_loop
  ).

% Invierte el valor de turn
% 0: oponente; 1: jugador
next_turn:-
	turn(Turn),
	New_turn is (Turn+1) mod 2,
	retract(turn(Turn)),
	assert(turn(New_turn)).

% Termina el juego y revisa quién ganó
end_game:-
	game_state(num_player_tiles, Num_player_tiles),
	game_state(num_oppo_tiles, Num_oppo_tiles),
	(	Num_player_tiles = 0 ->
			writeln("Ganaste");
		Num_oppo_tiles = 0 ->
			writeln("Ganó el oponente");
			writeln("Empataron")
	),
	write("------------- Fin del juego ----------------").

% ------------------------- Jugador -------------------------
% Lleva a cabo un turno del jugador (excepto el primero del juego) 
player_turn:-
	nl, write("---------Turno del jugador--------"), nl,
	game_state(player_hand, Player_hand),
	include(tile_has_legal_move, Player_hand, Player_legal_tiles), % filtra qué fichas se pueden tirar
	length(Player_legal_tiles, Num_legal_tiles),
	game_state(stock_size, Stock_size),
	game_state(player_passed, Player_passed),
	(	Num_legal_tiles = 0 ->
			(	Stock_size > 0 ->
					% Si quedan fichas, roba y vuelve a intentar
					player_steal, player_turn, !;
					% En caso contrario, pierde el turno
					write("El jugador pasa"), nl,
					% Actualiza si el jugador pasó o no
					(	Player_passed =:= 0 -> 
							New_player_passed is 1,
							retract( game_state(player_passed, Player_passed) ),
							assert( game_state(player_passed, New_player_passed) ); !
					), !
			);
			% Actualiza si el jugador pasó o no
			(	Player_passed =:= 1 -> 
					New_player_passed is 0,
					retract(game_state(player_passed, Player_passed)),
					assert(game_state(player_passed, New_player_passed)); !
			),
			% Escoge una jugada (si tiene jugadas legales)
			%player_choose_move(Player_legal_tiles, Player_tile, Open_tile_N),
			player_choose_move(Player_tile, Open_tile_N),
			place_tile(Player_tile, Open_tile_N),
			write("Coloca "), write(Player_tile), write(" junto a "), write(Open_tile_N), nl
	),
	% TODO: esto se imprime varias veces si robamos
	write("---------Fin de turno del jugador--------"), nl, nl.

% Roba del pozo si no tiene jugadas legales
player_steal:-
	writeln("Roba una ficha. ¿Cuál salió?"),
	read(Tile),
	order_input(Tile, Ordered_tile),
	(
		% Revisa la entrada
		reenter_input(Ordered_tile, player_steal);
		tile_in_stock_or_oppo(Ordered_tile, player_steal);
		
		% Agrega la ficha que tomó el jugador del pozo a su mano
		add_player_tile(Ordered_tile)
	).

% ------------------------- Oponente -------------------------
% Lleva a cabo un turno del oponente (excepto el primero del juego)
oppo_turn:-
	nl, write("---------Turno del oponente---------"), nl,
	oppo_steal,
	oppo_pick_tile(Oppo_tile),
	game_state(oppo_passed, Oppo_passed),
	(	Oppo_tile \= n ->
			% Si no pasó
			oppo_pick_open(Oppo_tile, Open_tile),
			place_tile(Oppo_tile, Open_tile),
			% Actualiza si el oponente pasó o no
			(	Oppo_passed =:= 1 -> 
					New_oppo_passed is 0,
					retract(game_state(oppo_passed, Oppo_passed)),
					assert(game_state(oppo_passed, New_oppo_passed)); !
			), !;
			
			% Si sí pasó
			write("El oponente pasa"), nl,
			% Actualiza si el oponente pasó o no
			(	Oppo_passed =:= 0 -> 
					New_oppo_passed is 1,
					retract( game_state(oppo_passed, Oppo_passed) ),
					assert( game_state(oppo_passed, New_oppo_passed) ); !
			)
	),
	write("---------Fin de turno del oponente---------"), nl, nl.

% En caso de que robe fichas del pozo
oppo_steal:-
	game_state(open_tiles, [Open_tile_N1, Open_tile_N2]),
	game_state(stock_size, Stock_size),
	game_state(num_oppo_tiles, Num_oppo_tiles),
	game_state(oppo_missing, Oppo_missing),
	write("¿El oponente robó del pozo? 's.' o 'n.'"), nl,
	read(Oppo_stock),
	(
		reenter_input(Oppo_stock, oppo_steal);
		
		(	Oppo_stock = n ->
				! % si no tomó del pozo, regresa verdadero
		);
		
		% Si sí tomó, revisa cuántas
		write("¿Cuántas?"), nl, 
		read(Oppo_stock_num), 
		(
			reenter_input(Oppo_stock_num, oppo_steal);
			
			(	Stock_size < Oppo_stock_num ->
					write("No hay suficientes fichas en el pozo"), nl, oppo_steal, !
			);
			
			% Suma las fichas que tomó el oponente del pozo a su mano
			New_num_oppo_tiles is Num_oppo_tiles + Oppo_stock_num,
			retract(game_state(num_oppo_tiles, Num_oppo_tiles)),
			assert(game_state(num_oppo_tiles, New_num_oppo_tiles)),
			% Resta las fichas que tomó el oponente del pozo
			New_stock_size is Stock_size - Oppo_stock_num,
			retract(game_state(stock_size, Stock_size)),
			assert(game_state(stock_size, New_stock_size)),
			% Actualiza los números que el oponente no tiene
			(	not(member(Open_tile_N1, Oppo_missing)) ->
					append([Open_tile_N1], Oppo_missing, Oppo_missing1), !;
					Oppo_missing1 = Oppo_missing % si ya está, no le agrega nada
			),
			(	not(member(Open_tile_N2, Oppo_missing1)) ->
					append([Open_tile_N2], Oppo_missing1, Oppo_missing2), !;
					Oppo_missing2 = Oppo_missing1 % si ya está, no le agrega nada
			),
			retract(game_state(oppo_missing, Oppo_missing)),
			assert(game_state(oppo_missing, Oppo_missing2))
		)
	).

% Selecciona qué ficha tiró el oponente
% oppo_pick_tile(o)
oppo_pick_tile(Oppo_tile):-
	game_state(rem_tiles, Rem_tiles),
	write("¿Qué tiró el oponente? Si no tuvo jugadas legales, escribe 'n.'"), nl,
	read(Tile),
	order_input(Tile, Ordered_tile),
	(
		reenter_input(Ordered_tile, oppo_pick_tile(Oppo_tile));
		
		( Ordered_tile \= n, not(member(Ordered_tile, Rem_tiles)) ->
				write("La ficha no está disponible"), nl, oppo_pick_tile(Oppo_tile), !
		);
		Oppo_tile = Ordered_tile % iguala Oppo_tile a la ficha leída y la manda de regreso a oppo_turn
	).

% Elige junto a cuál de las fichas abiertas jugó
% oppo_pick_open(i, o)
oppo_pick_open(Oppo_tile, Open_tile_N):-
	game_state(open_tiles, [Open_tile_N1, Open_tile_N2]),
	(	(not(legal_move(Oppo_tile, Open_tile_N1)); Open_tile_N1 =:= Open_tile_N2) ->
			% Si sólo hay una jugada legal
			Open_tile_N = Open_tile_N2, !;
		not(legal_move(Oppo_tile, Open_tile_N2)) ->
			% Si sólo hay una jugada legal
			Open_tile_N = Open_tile_N1, !;
			
			% Si puede jugar en ambos lados
			write("Las fichas abiertas son: "), 
			write(Open_tile_N1), write("  "), write(Open_tile_N2), nl,	
			write("¿Junto a cuál ficha abierta tiró el oponente?"),
			read(Open),
			(
				reenter_input(Open, oppo_pick_open(Oppo_tile, Open_tile_N));
				
				(	(Open = Open_tile_N1; Open = Open_tile_N2) ->
						Open_tile_N = Open, !; % iguala Open_tile_N a la ficha leída y la manda de regreso
						writeln("No elegiste una ficha abierta en el tablero"),
						oppo_pick_open(Oppo_tile, Open_tile_N)
				)
			)
	).

% Actualiza el estado del juego después de colocar una ficha
% place_tile(i, i)
place_tile((Tile_N1, Tile_N2), Open_tile_N):-
	turn(Turn),
	(	Turn = 0 ->
		% Actualiza cuántas fichas tiene el oponente
		game_state(num_oppo_tiles, Num_oppo_tiles),
		New_num_oppo_tiles is Num_oppo_tiles - 1,
		retract(game_state(num_oppo_tiles, Num_oppo_tiles)),
		assert(game_state(num_oppo_tiles, New_num_oppo_tiles)), !;
		
		% Actualiza cuántas fichas (y cuáles) tiene el jugador
		game_state(num_player_tiles, Num_player_tiles),
		New_num_player_tiles is Num_player_tiles - 1,
		retract(game_state(num_player_tiles, Num_player_tiles)),
		assert(game_state(num_player_tiles, New_num_player_tiles)),
		game_state(player_hand, Player_hand),
		delete(Player_hand, (Tile_N1, Tile_N2), New_player_hand),
		retract(game_state(player_hand, Player_hand)),
		assert(game_state(player_hand, New_player_hand))
	),
	
	% Actualiza qué fichas quedan
	game_state(rem_tiles, Rem_tiles),
	delete(Rem_tiles, (Tile_N1, Tile_N2), New_rem_tiles),
	retract(game_state(rem_tiles, Rem_tiles)),
  assert(game_state(rem_tiles, New_rem_tiles)),
	
	% Actualiza las fichas abiertas
	game_state(open_tiles, [Open_tile_N1, Open_tile_N2]),
	retract(game_state(open_tiles, [Open_tile_N1, Open_tile_N2])),
	%delete(Open_tiles, Open_tile_N, Open_tiles_short),
	
	(	Open_tile_N1 = Open_tile_N ->
		append([], [Open_tile_N2], Open_tiles_short), !; % Open_tiles_short = [Open_tile_N2]
		append([], [Open_tile_N1], Open_tiles_short)     % Open_tiles_short = [Open_tile_N1]
	),
	
	(	Tile_N1 =:= Open_tile_N -> % revisa qué lado de la ficha queda abierto
			% El lado 2 queda abierto
			append(Open_tiles_short, [Tile_N2], New_open_tiles), !;
			% El lado 1 queda abierto
			append(Open_tiles_short, [Tile_N1], New_open_tiles)
	),
  assert(game_state(open_tiles, New_open_tiles)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%   Utilidades   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Se asegura de que las fichas que se ingresen tengan el número menor a la izquierda
% order_input_aux(i, o)
order_input_aux(Out, Out). % Iguala Out a la entrada
% order_input(i, o)
order_input((N1, N2), Out):-
  ( N1 > N2 ->
    % reordena
    order_input_aux((N2, N1), Out), !; % el ! es por eficiencia
    % deja el orden igual
    order_input_aux((N1, N2), Out)
  ).
% Si no entra en los casos anteriores, la entrada es errónea;
% regresa true y deja que el método principal lo maneje
order_input(Out, Out).

% Permite reingresar una entrada si se cometió un error
% reenter_input_(i, i)
reenter_input(Inp, Method):-
	write("Ingresaste: "), write(Inp), 
  write("; escribe 'n.' para reintentar o 's.' para confirmar"), nl,
  read(Reenter),
  (	Reenter = n ->
  		% Vuelve a llamar al método si ingresó 'n'; si no, regresa false
  		Method, !
  ).

% Quita una ficha del pool disponible
% remove_tile(i)
remove_tile(Tile):-
	game_state(rem_tiles, Rem_tiles),
	delete(Rem_tiles, Tile, New_rem_tiles),
	retract(game_state(rem_tiles, Rem_tiles)),
	assert(game_state(rem_tiles, New_rem_tiles)).

% Agrega una ficha a la mano del jugador
% add_player_tile(i)
add_player_tile(Tile):-
 	game_state(rem_tiles, Rem_tiles),
 	game_state(stock_size, Stock_size),
  game_state(player_hand, Player_hand),
  game_state(num_player_tiles, Num_player_tiles),
	% Actualiza las fichas disponibles
	delete(Rem_tiles, Tile, New_rem_tiles),
	retract(game_state(rem_tiles, Rem_tiles)),
	assert(game_state(rem_tiles, New_rem_tiles)),
	% Actualiza el número de fichas en el pozo
	New_stock_size is Stock_size-1,
	retract(game_state(stock_size, Stock_size)),
	assert(game_state(stock_size, New_stock_size)),
	% Actualiza la mano del jugador
	append([Tile], Player_hand, New_player_hand),
	retract(game_state(player_hand, Player_hand)),
	assert(game_state(player_hand, New_player_hand)),
	% Actualiza el número de fichas del jugador
	New_num_player_tiles is Num_player_tiles+1,
	retract(game_state(num_player_tiles, Num_player_tiles)),
	assert(game_state(num_player_tiles, New_num_player_tiles)).

% Si una ficha no está en el pozo ni la mano del oponente, vuelve a correr la cláusula que lo llamó;
% en caso contrario, regresa false
% tile_in_stock_or_oppo(i, i)
tile_in_stock_or_oppo(Tile, Method):-
	game_state(rem_tiles, Rem_tiles),
	(	not(member(Tile, Rem_tiles)) ->
			writeln("La ficha ingresada no está disponible"), Method, !
	).

% Si una ficha ya se jugó o no existe, vuelve a correr la cláusula que lo llamó;
% en caso contrario, regresa false
% tile_exists(i, i)
tile_exists(Tile, Method):-
	game_state(rem_tiles, Rem_tiles),
	game_state(player_hand, Player_hand),
	(	(not(member(Tile, Rem_tiles)), not(member(Tile, Player_hand))) ->
			writeln("La ficha ingresada no está disponible"), Method, !
	).

% Regresa true si la ficha puede colocarse junto a la ficha abierta dada
% legal_move(i, i)
legal_move((Tile_N1, Tile_N2), Open_tile_N):-
	Open_tile_N =:= Tile_N1;
	Open_tile_N =:= Tile_N2.
% Igual que el método anterior, pero con una firma diferente por flexibilidad
% legal_move2(i, i)
legal_move2(Open_tile_N, (Tile_N1, Tile_N2)):-
	Open_tile_N =:= Tile_N1;
	Open_tile_N =:= Tile_N2.

% Regresa true si la ficha dada puede colocarse en algún lado del tablero
% tile_has_legal_move(i)
tile_has_legal_move((Tile_N1, Tile_N2)):-
	game_state(open_tiles, [Open_tile_N1, Open_tile_N2]),
	(	Open_tile_N1 =:= Tile_N1;
		Open_tile_N1 =:= Tile_N2;
		Open_tile_N2 =:= Tile_N1;
		Open_tile_N2 =:= Tile_N2).

% Imprime toda la información sobre el estado del juego
print_game_state:-
	nl, write("---Estado del juego---"), nl,
	game_state(player_hand, Player_hand),
	include(tile_has_legal_move, Player_hand, Player_legal_tiles),
	game_state(num_player_tiles, Num_player_tiles),
	game_state(num_oppo_tiles, Num_oppo_tiles),
	game_state(player_passed, Player_passed),
	game_state(oppo_missing, Oppo_missing),
	game_state(oppo_passed, Oppo_passed),
	game_state(open_tiles, Open_tiles),
	game_state(stock_size, Stock_size),
	game_state(rem_tiles, Rem_tiles),
	
	print_player_hand,
	write("Fichas legales: "), write(Player_legal_tiles), nl,
	write("El jugador tiene "), write(Num_player_tiles), write(" fichas"), nl,
	write("El oponente tiene "), write(Num_oppo_tiles), write(" fichas"), nl,
	write("Fichas abiertas: "), write(Open_tiles), nl,
	write("Hay "), write(Stock_size), write(" fichas en el pozo"), nl,
	print_list(Oppo_missing, "El oponente no tiene: "),
	
	write("El jugador pasó: "),
	(	Player_passed =:= 1 ->
			write("Sí"), nl, !; write("No"), nl
	),
	write("El oponente pasó: "),
	(	Oppo_passed =:= 1 ->
			write("Sí"), nl, !; write("No"), nl
	),
	
	write("Fichas del oponente/pozo: "), write(Rem_tiles), nl,
	write("---Fin de estado del juego---"), nl.

% Imprime la mano del jugador
print_player_hand:-
	game_state(player_hand, Player_hand),
	print_list(Player_hand, "Tus fichas:").

% Imprime cualquier lista junto con un mensaje apropiado
% print_list(i, i)
print_list(List, Message):-
	write(Message), nl,
	print_list(List), nl.
print_list([]):-nl.
print_list([Elem|Rest]):-
	write(Elem), write("   "),
	print_list(Rest).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%   Toma de decisiones   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Escoge qué jugada hacer
% player_choose_move(i, o, o)
player_choose_move(Ch_player_tile, Ch_open_tile_N):-
	game_state(rem_tiles, Rem_tiles),
	game_state(player_hand, Player_hand),
	game_state(open_tiles, Open_tiles),
	game_state(num_player_tiles, Num_player_tiles),
	game_state(num_oppo_tiles, Num_oppo_tiles),
	game_state(stock_size, Stock_size),
	game_state(oppo_missing, Oppo_missing),
	game_state(oppo_passed, Oppo_passed),
	game_state(player_passed, Player_passed),
	
	% Proba inicial es 1
	Node = [Rem_tiles, Player_hand, Open_tiles, Num_player_tiles, Num_oppo_tiles, Stock_size, Oppo_missing, Oppo_passed, Player_passed, 1],
	
	% Encuentra todas las jugadas legales
	findall([Player_tile, Open_tile_N], is_legal_move(Player_hand, Open_tiles, [Player_tile, Open_tile_N]), P_legal_moves),
	sort_moves(Node, P_legal_moves, P_sorted_moves), % ordena nodos
	% Encuentra los hijos del nodo actual
	maplist(find_child(Node, 1), P_sorted_moves, Children),
	
	% Asigna a cada jugada su evaluación
	maplist(map_eval, Children, P_sorted_moves, Move_eval_pairs),
	% Ordena por heurística (de manera ascendente)
	keysort(Move_eval_pairs, Sorted_pairs_),
	% Reordena de manera descendente
	reverse(Sorted_pairs_, Sorted_pairs),
	% Coloca las jugadas en Ordered_moves
	maplist(pair_second, Sorted_pairs, Ordered_moves),
	% La primera de la lista es la mejor jugada
	nth0(0, Ordered_moves, Best_move),
	nth0(0, Best_move, Ch_player_tile),
	nth0(1, Best_move, Ch_open_tile_N).

% Placeholder
player_choose_move_([(Tile_N1, Tile_N2)|_], Player_tile, Open_tile_N):-
	% Escoge la primera ficha de la lista
	Player_tile = (Tile_N1, Tile_N2),
	
	game_state(open_tiles, [Open_tile_N1, Open_tile_N2]),
	(	Tile_N1 = Open_tile_N1; Tile_N2 = Open_tile_N1 ->
		Open_tile_N = Open_tile_N1, !;
		Open_tile_N = Open_tile_N2
	).

% Heurística: regresa la evaluación de un nodo hoja
% eval_node(i, o)
eval_node(Node, Eval):-
	nth0(3, Node, Num_player_tiles),
	nth0(4, Node, Num_oppo_tiles),
	nth0(7, Node, Oppo_passed),
	nth0(8, Node, Player_passed),
	nth0(9, Node, Prob),
	(	Num_player_tiles = 0 ->
			% El jugador gana
			Eval is inf, !;
		Num_oppo_tiles = 0 ->
			% El oponente gana
			Eval is -inf, !;
			% Si ningún jugador ha ganado
		(Oppo_passed =:= 1, Player_passed =:= 1) ->
			% En este caso, se empata
			Eval is 0, !;
			% Si no entra a ninguno de los anteriores
			Eval is Prob * (Num_oppo_tiles - Num_player_tiles)
	).

% Árbol minimax
% Node = [Rem_tiles, Player_hand, Open_tiles, Num_player_tiles, Num_oppo_tiles, Stock_size,
%         Oppo_missing, Oppo_passed, Player_passed, Prob]
% minimax(i, i, i, o)
minimax(Node, Depth, Max_player, Eval):-
	nth0(3, Node, Num_player_tiles),
	nth0(4, Node, Num_oppo_tiles),
	nth0(7, Node, Oppo_passed),
	nth0(8, Node, Player_passed),
	(	(Depth =:= 0; Num_player_tiles =:= 0; Num_oppo_tiles =:= 0; (Oppo_passed =:= 1, Player_passed =:= 1)) ->
		% Evalúa si Depth es 0 o el juego termina
		eval_node(Node, Eval), !;
		
		% En caso contrario, continúa explorando
		Depth1 is Depth-1,
		find_children(Node, Max_player, Children),
		(	Max_player =:= 1 -> 
				% Si es turno del jugador
				minimax_max_player(Children, Depth1, -inf, Eval), !;
				
				% Si es turno del oponente
				minimax_min_player(Children, Depth1, inf, Eval)
		)
	).

% Métodos auxiliares para minimax
% minimax_max_player(i, i, i, o)
minimax_max_player([], _, Eval, Eval):- !. % cuando ya no quedan hijos, para
minimax_max_player([Child|Rest], Depth, Prev_eval, Eval):-
	minimax(Child, Depth, 0, New_eval),
	( Prev_eval < New_eval ->
			minimax_max_player(Rest, Depth, New_eval, Eval), !;
			minimax_max_player(Rest, Depth, Prev_eval, Eval)
	).
% minimax_min_player(i, i, i, o)
minimax_min_player([], _, Eval, Eval):- !. % cuando ya no quedan hijos, para
minimax_min_player([Child|Rest], Depth, Prev_eval, Eval):-
	minimax(Child, Depth, 1, New_eval),
	( Prev_eval > New_eval ->
			minimax_min_player(Rest, Depth, New_eval, Eval);
			minimax_min_player(Rest, Depth, Prev_eval, Eval)
	).

% Node = [Rem_tiles, Player_hand, Open_tiles, Num_player_tiles, Num_oppo_tiles, Stock_size,
%         Oppo_missing, Oppo_passed, Player_passed, Prob]
% find_children(i, i, o)
find_children(Node, Max_player, Children):-
	nth0(2, Node, Open_tiles),
	(	Max_player =:= 1 ->
			nth0(1, Node, Player_hand),
			% Encuentra todas las jugadas legales de la forma [Tile, Open_tile_N]
			findall([Player_tile, Open_tile_N], is_legal_move(Player_hand, Open_tiles, [Player_tile, Open_tile_N]), P_legal_moves),
			sort_moves(Node, P_legal_moves, P_sorted_moves), % ordena nodos
			% Encuentra los hijos del nodo actual
			maplist(find_child(Node, Max_player), P_sorted_moves, Children), 
			% Agrega el caso en que el jugador roba
			%append([P_steal_node], Children_, Children),
			!;
			
			nth0(0, Node, Rem_tiles),
			findall([Oppo_tile, Open_tile_N], is_legal_move(Rem_tiles, Open_tiles, [Oppo_tile, Open_tile_N]), O_legal_moves),
			sort(O_legal_moves, O_sorted_moves), % elimina jugadas repetidas
			maplist(find_child(Node, Max_player), O_sorted_moves, Children)
			% Agrega el caso en que el oponente pasa
			%find_steal_child(Node, Max_player, , O_steal_node),
			%append([O_steal_node], Children_, Children)
	).

% Construye el hijo de un nodo, suponiendo que alguien robó
% find_steal_child(i, i, i, o)
find_steal_child(Node, Max_player, Child):-
	nth0(0, Node, Rem_tiles),
	nth0(1, Node, Player_hand),
	nth0(2, Node, [Open_tile_N1, Open_tile_N2]),
	nth0(3, Node, Num_player_tiles),
	nth0(4, Node, Num_oppo_tiles),
	nth0(5, Node, Stock_size),
	nth0(6, Node, Oppo_missing),
	nth0(7, Node, Oppo_passed),
	nth0(8, Node, Player_passed),
	nth0(9, Node, Prob),
	(	Max_player =:= 1 ->
			% Suma las fichas que se espera robar y agrega una ficha legal aleatoria
			Mod_node = [], !;
			!
	),
	find_child(Node, Max_player, [(Tile_N1, Tile_N2), Open_tile_N], Child).

% Encuentra el hijo de un nodo después de cierta jugada
% find_child(i, i, i, o)
find_child(Node, Max_player, [(Tile_N1, Tile_N2), Open_tile_N], Child):-
	nth0(0, Node, Rem_tiles),
	nth0(1, Node, Player_hand),
	nth0(2, Node, [Open_tile_N1, Open_tile_N2]),
	nth0(3, Node, Num_player_tiles),
	nth0(4, Node, Num_oppo_tiles),
	nth0(5, Node, Stock_size),
	nth0(6, Node, Oppo_missing),
	nth0(7, Node, Oppo_passed),
	nth0(8, Node, Player_passed),
	nth0(9, Node, Prob),
	
	% Encuentra qué fichas quedan abiertas
	(	Open_tile_N1 = Open_tile_N ->
		append([], [Open_tile_N2], Open_tiles_short), !; % Open_tiles_short = [Open_tile_N2]
		append([], [Open_tile_N1], Open_tiles_short)     % Open_tiles_short = [Open_tile_N1]
	),
	(	Tile_N1 =:= Open_tile_N -> % revisa qué lado de la ficha queda abierto
			% El lado 2 queda abierto
			append(Open_tiles_short, [Tile_N2], New_open_tiles), !;
			% El lado 1 queda abierto
			append(Open_tiles_short, [Tile_N1], New_open_tiles)
	),
	
	% Encuentra Child
	(	Max_player =:= 1 ->
			% Jugador
			delete(Player_hand, (Tile_N1, Tile_N2), New_player_hand),
			New_num_player is Num_player_tiles-1,
			Child = [Rem_tiles, New_player_hand, New_open_tiles, New_num_player, Num_oppo_tiles, Stock_size, Oppo_missing, Oppo_passed, Player_passed, Prob], !;
			
			% Oponente
			delete(Rem_tiles, (Tile_N1, Tile_N2), New_rem_tiles),
			New_num_oppo is Num_oppo_tiles-1,
			prob_move(Node, [(Tile_N1, Tile_N2), Open_tile_N], Prob_move),
			New_prob is Prob * Prob_move,
			Child = [New_rem_tiles, Player_hand, New_open_tiles, Num_player_tiles, New_num_oppo, Stock_size, Oppo_missing, Oppo_passed, Player_passed, New_prob]
	).

% Árbol minimax con poda alpha-beta
% minimax alpha_beta(i, i, i, i, i, o)
minimax_alpha_beta(Node, Depth, Max_player, Alpha, Beta, Eval):-
	nth0(3, Node, Num_player_tiles),
	nth0(4, Node, Num_oppo_tiles),
	nth0(7, Node, Oppo_passed),
	nth0(8, Node, Player_passed),
	(	(Depth =:= 0; Num_player_tiles =:= 0; Num_oppo_tiles =:= 0; (Oppo_passed =:= 1, Player_passed =:= 1)) ->
		% Evalúa si Depth es 0 o el juego termina
		eval_node(Node, Eval), !;
		
		% En caso contrario, continúa explorando
		Depth1 is Depth-1,
		find_children(Node, Max_player, Children),
		(	Max_player =:= 1 ->
				% Si es turno del jugador
				minimax_a_b_max_player(Children, Depth1, Alpha, Beta, -inf, Eval), !;
				
				% Si es turno del oponente
				minimax_a_b_min_player(Children, Depth1, Alpha, Beta, inf, Eval)
		)
	).

% Métodos auxiliares para minimax_alpha_beta
% minimax_a_b_max_player(i, i, i, i, i, o)
minimax_a_b_max_player([], _, _, _, Eval, Eval):- !. % cuando ya no quedan hijos, para
minimax_a_b_max_player([Child|Rest], Depth, Alpha, Beta, Prev_eval, Eval):-
	minimax_alpha_beta(Child, Depth, 0, Alpha, Beta, New_eval),
	Alpha1 is max(Alpha, New_eval),
	(	Beta =< Alpha1 ->
				!; % se sale
		( Prev_eval < New_eval->
				minimax_a_b_max_player(Rest, Depth, Alpha1, Beta, New_eval, Eval), !;
				minimax_a_b_max_player(Rest, Depth, Alpha1, Beta, Prev_eval, Eval)
		)
	).
% minimax_a_b_min_player(i, i, i, i, i, o)
minimax_a_b_min_player([], _, _, _, Eval, Eval):- !. % cuando ya no quedan hijos, para
minimax_a_b_min_player([Child|Rest], Depth, Alpha, Beta, Prev_eval, Eval):-
	minimax_alpha_beta(Child, Depth, 0, Alpha, Beta, New_eval),
	Beta1 is min(Beta, New_eval),
	(	Beta1 =< Alpha ->
				!; % se sale
		( Prev_eval > New_eval->
				minimax_a_b_max_player(Rest, Depth, Alpha, Beta1, New_eval, Eval), !;
				minimax_a_b_max_player(Rest, Depth, Alpha, Beta1, Prev_eval, Eval)
		)
	).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%   Utilidades de toma de decisiones   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Asigna a cada jugada la pareja Eval-Move
% map_eval(i, i, o)
map_eval(Node, Move, Move_eval_pair):-
	minimax_depth(Depth), % consulta la profundidad
	minimax(Node, Depth, 0, Eval),
	%minimax_alpha_beta(Node, Depth, 0, -inf, inf, Eval),
	Move_eval_pair = Eval-Move.

% Ordena las jugadas legales por prioridad
% sort_moves(i, i, o)
sort_moves(Node, Legal_moves_, Ordered_moves):-
	% Elimina jugadas repetidas
	sort(Legal_moves_, Legal_moves),
	% Mappea Move a la pareja Score-Move
	maplist(map_score(Node), Legal_moves, Move_score_pair),
	% Ordena por puntaje (de manera ascendente)
	keysort(Move_score_pair, Sorted_pairs_),
	% Reordena de manera descendente
	reverse(Sorted_pairs_, Sorted_pairs),
	% Coloca las jugadas en Ordered_moves
	maplist(pair_second, Sorted_pairs, Ordered_moves).

% Mappea Move a la pareja Score-Move
% map_score(i,i, o) 
map_score(Node, Move, Move_score_pair):-
	score_move(Node, Move, Score),
	Move_score_pair = Score-Move.

% Asigna un puntaje a una jugada para ordenarlas
%score_move(i, i, o)
score_move(Node, [(Player_tile_N1, Player_tile_N2), Open_tile_N], Score):-
	nth0(1, Node, Player_hand),
	nth0(2, Node, [Open_tile_N1, Open_tile_N2]),
	nth0(6, Node, Oppo_missing),
	% Criterio 1: la ficha es mula
	(	Player_tile_N1 =:= Player_tile_N2 ->
			Score1 is 100, !;
			Score1 is 0
	),
	% Criterio 2: esta jugada hace que los extremos que quedan abiertos sean números
	% que el oponente no tiene
	% Primero, encuentra qué números van a quedar abiertos
	(	Open_tile_N = Open_tile_N1 ->
			% Se coloca la ficha del lado 1
			(	Player_tile_N1 = Open_tile_N1 ->
					New_open = [Open_tile_N2, Player_tile_N2], !;
					New_open = [Open_tile_N2, Player_tile_N1]
			), !;
			% Se coloca la ficha del lado 2
			(	Player_tile_N1 = Open_tile_N2 ->
					New_open = [Open_tile_N1, Player_tile_N2], !;
					New_open = [Open_tile_N1, Player_tile_N1]
			)
	),
	(	subset(New_open, Oppo_missing) ->
			% Si el oponente no tiene ninguno de los nuevos números abiertos
			Score2 is 50, !;
			Score2 is 0
	),
	% Criterio 3: esta jugada nos garantiza otra jugada legal en el siguiente turno
	delete(Player_hand, (Player_tile_N1, Player_tile_N2), New_player_hand), % quita la ficha que se tira
	nth0(0, New_open, New_open_N1),
	nth0(1, New_open, New_open_N2),
	% Fichas que se pueden poner a la izquierda
	include(legal_move2(New_open_N1), New_player_hand, Legal1),
	length(Legal1, Num_legal1),
	% Fichas que se pueden poner a la izquierda
	include(legal_move2(New_open_N2), New_player_hand, Legal2),
	length(Legal2, Num_legal2),
	% Si se puede colocar al menos una ficha de cada lado, no nos pueden bloquear
	(	(Num_legal1 > 0, Num_legal2 > 0) ->
			Score3 is 10, !;
			Score3 is 0
	),
	Score is Score1 + Score2 + Score3.

% Obtiene el segundo elemento de una pareja
% pair_second(i, o).
pair_second(_-X, X).

% Regresa true si la jugada dada es legal i.e. la ficha abierta dada es válida y la ficha
% que se juega pertenece al pool correspondiente y puede colocarse junto a la ficha abierta
% is_legal_move(i, i, i)
is_legal_move(Tile_pool, Open_tiles, [(Tile_N1, Tile_N2), Open_tile_N]):-
	member((Tile_N1, Tile_N2), Tile_pool),
	member(Open_tile_N, Open_tiles),
	(	Tile_N1 = Open_tile_N;
		Tile_N2 = Open_tile_N
	).

% Regresa la proba de que el oponente haga cierta jugada, que es la proba de
% que tenga la ficha correspondiente dividida entre la cantidad de jugadas
% que puede hacer con ella.
% La proba de que el oponente tenga cierta ficha es k/k+m,
% donde k es el número de fichas en su mano y m el número en el pozo.
% prob_move(i, i, o)
prob_move(Node, [(Oppo_tile_N1, Oppo_tile_N2), _], Prob):-
	nth0(2, Node, [Open_tile_N1, Open_tile_N2]),
	nth0(4, Node, Num_oppo_tiles), % k
	nth0(5, Node, Stock_size), % m
	% Cuenta cuántas jugadas son posibles con esta ficha
	(	Oppo_tile_N1 = Open_tile_N1 -> Count1 is 1, !; Count1 is 0),
	(	Oppo_tile_N1 = Open_tile_N2 -> Count2 is 1, !; Count2 is 0),
	(	Oppo_tile_N2 = Open_tile_N1 -> Count3 is 1, !; Count3 is 0),
	(	Oppo_tile_N2 = Open_tile_N2 -> Count4 is 1, !; Count4 is 0),
	% Asumimos que todas las jugadas con esta ficha son equiprobables
	Prob is (Num_oppo_tiles/(Num_oppo_tiles + Stock_size)) / (Count1 + Count2 + Count3 + Count4).

% Calcula la proba de que el oponente tenga que robar en este turno, que es
% m+k-l choose k / m+k choose k
% prob_oppo_steal(i, o)
prob_oppo_steal(Node, Prob):-
	nth0(0, Node, Rem_tiles),
	nth0(4, Node, Oppo_num_tiles), % k
	nth0(5, Node, Stock_size), % m
	Total_tiles is Oppo_num_tiles + Stock_size, % m+k
	include(tile_has_legal_move, Rem_tiles, Oppo_legal_tiles), % filtra las fichas legales
	length(Oppo_legal_tiles, Num_legal), % l = Num_legal
	Num_illegal is Stock_size - Num_legal, % m-l
	% La proba es m-l choose k / m+k choose k
	bin_coeff(Num_illegal, Oppo_num_tiles, Illegal_hands), % m-l choose k
	bin_coeff(Total_tiles, Oppo_num_tiles, Total_hands), % m+k choose k
	Prob is Illegal_hands / Total_hands.

% Calcula la cantidad de fichas que se espera robe el oponente en promedio
% E = (k+m+1)/(k+1)
% oppo_expctd_steal(i, o):-
oppo_expctd_steal(Node, Expctd):-
	nth0(4, Node, Oppo_num_tiles), % k
	nth0(5, Node, Stock_size), % m
	Expctd is (Oppo_num_tiles + Stock_size + 1)/(Oppo_num_tiles + 1).

% Calcula n choose k = n!/k!(n-k)!
% bin_coeff(i, i, o)
bin_coeff(N, K, Res):-
	(	(N =:= 0; K =:= 0; N < K) ->
		Res is 0, !;
		N_minus_K is N - K,
		fact(N, Fact_N),
		fact(K, Fact_K),
		fact(N_minus_K, Fact_N_minus_K),
		Res is Fact_N / (Fact_K * Fact_N_minus_K)
	).

% Calcula el factorial
% fact(i, o)
fact(N, Res):-
	(	N =< 0 ->
			Res is 1, !;
			N1 is N-1,
			fact(N1, Res1),
			Res is N*Res1
	).
	
