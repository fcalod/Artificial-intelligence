% Polynomials p=an*x^n + ... + a1*x + a0 are represented 
% as [(an, n), ..., (a1, 1), (a, 0)]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%% Addition %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Adds two polynomials and stores the result in Res
%
% add_(i, i, o)
add([], [], Res):-
	% If both polynomials are done, initialize the result as
	% an empty list and let the recursion fill it up
	append([], Res).
% If the first polynomial is done, the branching method fails
add([], [(Coef2, Deg2)|Terms2], Res):-
	add_R([], [(Coef2, Deg2)|Terms2], Res).
% Same as above
add([(Coef1, Deg1)|Terms1], [], Res):-
	add_L([(Coef1, Deg1)|Terms1], [], Res).
% Branching method. Checks which degree is greater and 
% selects the corresponding operation
add([(Coef1, Deg1)|Terms1], [(Coef2, Deg2)|Terms2], Res):-
	(Deg1 > Deg2 ->
		% if the degree of the first term is greater, pop it and push it into Res
		add_L([(Coef1, Deg1)|Terms1], [(Coef2, Deg2)|Terms2], Res);
		(Deg1 < Deg2 ->
			add_R([(Coef1, Deg1)|Terms1], [(Coef2, Deg2)|Terms2], Res);
			(Deg1 =:= Deg2 ->
				add_E([(Coef1, Deg1)|Terms1], [(Coef2, Deg2)|Terms2], Res)
			)
		)
	).

% Auxiliary methods for adding
% If both degrees are equal, adds the coefficients, places the
% new term in Res and pops the original terms from both inputs
%
% add_E(i, i, o)
add_E([(Coef1, Deg)|Terms1], [(Coef2, Deg)|Terms2], [(Coef, Deg)|Res]):-
	Coef is Coef1+Coef2, add(Terms1, Terms2, Res).
% If the right degree is greater, pops the top term from the right
% polynomial into Res
%
% add_R(i, i, o)
add_R(Terms1, [(Coef, Deg)|Terms2], [(Coef, Deg)|Res]):-
	add(Terms1, Terms2, Res).
% Analogous to add_R
%
% add_L(i, i, o)
add_L([(Coef, Deg)|Terms1], Terms2, [(Coef, Deg)|Res]):-
	add(Terms1, Terms2, Res).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%% Substraction %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Substracts two polynomials
% 
% sub(i, i, o)
sub(P, Q, Res):-
	mult(Q, [(-1, 0)], Minus_Q),
	add(P, Minus_Q, Res).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%% Multiplication %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Multiplies two polynomials
%
% mult(i, i, o) 
mult(_,[],[]):- !.
mult(Poly, [(Coef, Exp)|Terms], Res):-
   mult(Poly, Terms, Rec),
   mult_term(Poly, Coef, Exp, Scalar),
   add(Scalar, Rec, Res),
   !.

% Multiplies a polynomial by a single term
% 
% mult_scalar(i, i, i, o) 
mult_term([],_,_,[]):- !.
mult_term([(Coef1, Deg1)|Terms1], Coef2, Deg2, [(Coef, Deg)|Terms2]):-
   Coef is Coef1*Coef2,
   Deg is Deg1+Deg2,
   mult_term(Terms1, Coef2, Deg2, Terms2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%% Dertivative %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Calcultes the derivative
%
% deriv(i, o)
deriv([], Res):-
	append([], Res).
deriv([(Coef1, Deg1)|Terms], [(Coef2, Deg2)|Res]):-
	(Deg1 > 0 ->
		% if the degree is > 0, apply the derivative rule for polynomials
		Coef2 is Coef1*Deg1,
		Deg2 is Deg1-1,
		deriv(Terms, Res);
		(	% otherwise, the term is a constant with derivative 0
			Coef2 is 0,
			Deg2 is 0,
			deriv([], Res)
		)
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%% Evaluation (and composition) %%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Evaluates the value of the polynomial Poly at X using Horner's rule.
% X must be a polynomial; for evluating constants, a constant polynomial
% (e.g. [(3, 0)]) should be used
% 
% eval(i, i, o)
eval(Poly, X, R):-
	eval(Poly, X, [(0 ,0)], R).
eval([], _, R, R).
eval([(Coef, _)|Terms], X, Acum, R):-
	mult(Acum, X, M),
	add(M, [(Coef, 0)], Z),
    eval(Terms, X, Z, R).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Printing %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Erases terms with coefficient 0
% 
% clean(i, o)
clean([], []).
clean([(Coef, Deg)|Terms], Clean_poly):-
	(Coef \= 0 ->
		clean_branch([(Coef, Deg)|Terms], Clean_poly)
	);
	clean(Terms, Clean_poly).
% Auxiliary method for clean
%
% clean_branch(i, o)
clean_branch([(Coef, Deg)|Terms], [(Coef, Deg)|Clean_poly]):-
	clean(Terms, Clean_poly).
	

% Prints a polynomial
% 
% print(i)
print(Poly):-
	length(Poly, Poly_length),
	(Poly_length > 1 ->
		% if there is more than one term, clean Poly before printing
		clean(Poly, Clean_poly),
		print_aux(Clean_poly)
	);
	% if there is a single term, print it
	print_aux(Poly).

% Auxiliary method for print
% 
% print_aux(i)
print_aux([]).
% Prints any other term
print_aux([(Coef, Deg)|Terms]):-
	length(Terms, Num_Remaining), % number of remaining terms
	%abs(Coef, Coef_abs), % 
	(Deg =:= 0 ->
		% if Deg is 0, print the coefficient
		write(Coef);
		(Deg =:= 1 ->
			% if Deg is 1, print "x" without its exponent
			write(Coef), write("x");
			% else, print "x^[Deg]"
			write(Coef), write("x^"), write(Deg)
		),
		(Num_Remaining > 0 ->
			% prints a "+" if there are more terms remaining
			write(" + ");
			% forces the program to run the recursive call, whether
			% or not the "+" was added
			true 
		)
	),
	print_aux(Terms).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Main %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

zero([(0,0)]).

% p = 4x³+3x²+2x+1
p1([(4,3), (0, 2), (0, 1), (0, 0)]).
p2([(3,2), (0, 1), (0, 0)]).
p3([(1,0)]).
p4([(2,1), (0, 0)]).
% q = 3x²+5
q1([(3,2), (0, 1), (0, 0)]).
q2([(5,0)]).

main:-
    zero(Z),
    p1(P1), p2(P2), add(P1, P2, D1),
    p3(P3), p4(P4), add(P4, P3, D2),
    q1(Q1), q2(Q2),

    write("prueba(x)   = "), print(P1), nl,
    write("zero(x)     = "), print(Z), nl,
    write("p(x)        = "), add(D1, D2, P), print(P), nl,
    write("q(x)        = "), add(Q1, Q2, Q), print(Q), nl,
    write("p(x) + q(x) = "), add(P, Q, P_plus_Q), print(P_plus_Q), nl,
    write("p(x) * q(x) = "), mult(P, Q, P_times_Q), print(P_times_Q), nl,
    write("p(q(x))     = "), eval(P, Q, P_eval_Q), print(P_eval_Q), nl,
    write("0 - p(x)    = "), sub(Z, P, Minus_P), print(Minus_P), nl,
    write("p(3)        = "), eval(P, [(3, 0)], P_eval_3), print(P_eval_3), nl,
    write("p'(x)       = "), deriv(P, P_dash), print(P_dash), nl,
    write("p''(x)      = "), deriv(P_dash, P_dbl_dash), print(P_dbl_dash).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
