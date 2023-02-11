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
add_poly(Poly1, [], Poly1).
add_poly([], Poly2, Poly2).
add_poly([(Coef1,Exp1)|Terms1], [(Coef2,Exp2)|Terms2], Result) :-
    ( 
    Exp1 =:= Exp2 ->
        Coef is Coef1 + Coef2,
        add_poly(Terms1, Terms2, NextRes),
        ( 
	Coef =\= 0 ->
            Res = [(Coef,Exp1)|NextRes];
            Res = NextRes
        );
        Exp1 < Exp2 ->
        add_poly(Terms1, [(Coef2,Exp2)|Terms2], NextRes),
        Res = [(Coef1,Exp1)|NextRes];
        Exp1 > Exp2 ->
        add_poly([(Coef1,Exp1)|Terms1], Terms2, NextRes),
        Res = [(Coef2,Exp2)|NextRes]
    ).

%%% eval_poly(Poly, X, Res)
% evaluates the value of Poly at X, returns Res
eval_poly([], _, 0).
eval_poly([(Coef,Exp)|Terms], X, Res) :-
    TermValue is Coef * X^Exp,
    eval_poly(Terms, X, NextRes),
    Res is TermValue + NextRes.

%% to_string(Poly)
% writes the polynomial Poly to the terminal
to_string([[C, D]]):-
    write(C),write("x^"),write(D).
to_string([[C, D]|Y]):-
    write(C),write("x^"),write(D), write(" + "), to_string(Y).

%%% mult_poly(Poly1, Poly2, Res) 
% multiplies 2 polynomials Poly1 and Poly2, returns Res
mult_poly(Poly1, [], []).
mult_poly([], Poly2, []).
mult_poly([(Coef1,Exp1)|Terms1], Poly2, Res) :-
    mult_poly_scalar((Coef1,Exp1), Poly2, PartialRes),
    mult_poly(Terms1, Poly2, NextRes),
    add_poly(PartialRes, NextRes, Res).

% mult_poly_scalar(Scalar, Poly, Res)
% mult_poly_term(Term, Poly, Result)
% multiplies a scalar Scalar and a polynomial Poly, returns Res
mult_poly_scalar((Coef,Exp), Poly, Res) :-
    maplist(mult_scalar(Coef,Exp), Poly, Res).  % maplist 

% mult_scalar(C, E, Scalar, Res)
% multiplies a coefficient Coef and an exponent Exp with a scalar Scalar, returns Res
mult_scalar(Coef, Exp, (Coef2,Exp2), (Coef3,Exp3)) :-
    Coef3 is Coef * Coef2,
    Exp3 is Exp + Exp2.


% sub(Poly1, Poly2, Res)  ________todavia le falta mult_poly para funcionar___________
% substracts 2 polynomials Poly1 and Poly2, returns Res
%subs_poly(Poly1, Poly2, Res):-
%	mult_poly(Poly2, [[-1, 0]], Poly1),
%	add_poly(Poly1, Poly2, Res).


