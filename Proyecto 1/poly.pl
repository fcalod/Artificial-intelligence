:- abolish(counter/1).
% the constant polynomial 0
zero([[0,0]]).

% Asserts Coef*x^Deg as being p
% In other words, "stores" Coef*x^Deg in p, while overriding
% the previous value
%
% p_ini(i, i)
p_ini(Coef, Deg):-
	retractall(p(_)),
	asserta(p([[Coef, Deg]])).

% Asserts Coef*x^Deg as being q
% In other words, "stores" Coef*x^Deg in q, while overriding
% the previous value
%
% q_ini(i, i)
q_ini(Coef, Deg):-
	retractall(q(_)),
	asserta(q([[Coef, Deg]])).

%%% add_poly(Poly1, Poly2, Res)
% adds 2 polynomials Poly1 and Poly2, returns Res
add_poly([], [], Res):-
   append([], Res).
add_poly([], [(Coef2, Exp2)|Terms], Res):-
   add_Right([], [(Coef2, Exp2)|Terms], Res).
add_poly([(Coef1, Exp1)|Terms], [], Res):-
   add_Left([(Coef1, Exp1)|Terms], [], Res).
add_poly([(Coef1, Exp1)|Terms1], [(Coef2, Exp2)|Terms2], Res):-
   (  Exp1 > Exp2 ->
      add_Left([(Coef1, Exp1)|Terms1], [(Coef2, Exp2)|Terms2], Res);
      (  Exp1 < Exp2 ->
         add_Right([(Coef1, Exp1)|Terms1], [(Coef2, Exp2)|Terms2], Res);
         (  Exp1 =:= Exp2 ->
            add_E([(Coef1, Exp1)|Terms1], [(Coef2, Exp2)|Terms2], Res)
         )
      )
   ).

add_E([(Coef1, Exp)|Terms1], [(Coef2, Exp)|Terms2], [(Coef, Exp)|Terms3]):-
   Coef is Coef1+Coef2, add_poly(Terms1, Terms2, Terms3).
add_Right(X, [(Coef, Exp)|Terms1], [(Coef, Exp)|Terms2]):-
   add_poly(X, Terms1, Terms2).
add_Left([(Coef, Exp)|Terms1], Y, [(Coef, Exp)|Terms2]):-
   add_poly(Terms1, Y, Terms2).

%%% eval_poly(Poly, X, Res)
% evaluates the value of the polynomial Poly at X, returns Res
eval_poly([], _, 0).
eval_poly([(Coef,Exp)|Terms], X, Res) :-
    TermValue is Coef * X^Exp,
    eval_poly(Terms, X, NextRes),
    Res is TermValue + NextRes.

%% to_string(Poly)
% writes the polynomial Poly to the terminal
to_string([(Coef, Exp)]):-
    write(Coef),write("x^"),write(Exp).
to_string([(Coef, Exp)|Terms]):-
    write(Coef),write("x^"),write(Exp), write(" + "), to_string(Terms).

%% mult_poly(Poly1, Poly2, Res) 
% multiplies 2 polynomials Poly1 and Poly2, returns Res
% (que es rec?)
mult_poly(_,[],[]):- !.
mult_poly(Poly, [(Coef, Exp)|Terms], Res):-
   mult_poly(Poly, Terms, Rec),
   mult_poly_scalar(Poly, Coef, Exp, Scalar),
   add_poly(Scalar, Rec, Res),
   !.

% mult_poly_scalar(Poly1, Esc, Coe, Res)
% multiplies a polynomial Poly1 with a scalar Scalar, returns Res
% la neta no entiendo que esta pasando aqui, pero intente refactorearlo
mult_poly_scalar([],_,_,[]):- !.
mult_poly_scalar([(Coef1, Exp1)|Terms1], Scalar, Coef, [(Coef2, Exp2)|Terms2]):-
   Coef2 is Coef1*Scalar,
   Exp2 is Exp1+Coef,
   mult_poly_scalar(Terms1, Scalar, Coef, Terms2).

%% subs(Poly1, Poly2, Res) 
% substracts 2 polynomials Poly1 and Poly2, returns Res
subs_poly(Poly1, Poly2, Res):-
	mult_poly(Poly2, [(-1, 0)], Poly2), 
	to_string(Poly1),
	add_poly(Poly1, Poly2, Res).

%% deriv(Poly, Res)
% takes the derivative of the polynomial Poly, returns Res
deriv_poly([], Res):-
	append([], Res).
deriv_poly([(Coef1, Exp1)|Terms1], [(Coef2, Exp2)|Terms2]):-
	(	
	Exp1 > 0 ->
		Coef2 is Coef1*Exp1,
		Exp2 is Exp1-1,
		deriv_poly(Terms1, Terms2);
		(
			Coef2 is 0,
			Exp2 is 0,
			deriv_poly([], Terms2)
		)
	).

%% comp_poly(Poly1, Poly2, Res)
% takes the composition of the polynomials Poly1 with Poly2, returns Res
compose_poly(Poly1, Poly2, Composed) :-
	%polynomial1(Poly1),
	%polynomial2(Poly2),
    compose_poly_helper(Poly1, Poly2, Composed).

compose_poly_helper(_, [], []).
compose_poly_helper(Poly1, [(Coef, Exp) | Terms], [R | RT]) :-
    eval_poly(Coef, Poly1, R),
    compose_poly_helper(Poly1, Terms, RT).
