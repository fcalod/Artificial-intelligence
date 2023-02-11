polynomial([(3, 2), (2, 1), (1, 0)]).

% Evaluate the polynomial for a given value of x
evaluate_polynomial(X, Poly, Result) :-
    polynomial(Poly),
    evaluate_polynomial_helper(X, Poly, Result, 0).

evaluate_polynomial_helper(_, [], Result, Result).
evaluate_polynomial_helper(X, [(Coef, Exp) | T], Result, Acc) :-
    NewAcc is Acc + Coef * (X ** Exp),
    evaluate_polynomial_helper(X, T, Result, NewAcc).



% Define two polynomials
polynomial1([(1, 2), (2, 1), (3, 0)]).
polynomial2([(2, 1), (1, 0)]).


% Compose the two polynomials
compose_polynomials(Poly1, Poly2, Composed) :-
    polynomial1(Poly1),
    polynomial2(Poly2),
    compose_polynomials_helper(Poly1, Poly2, Composed).

compose_polynomials_helper(_, [], []).
compose_polynomials_helper(Poly1, [(Coef, Exp) | T], [R | RT]) :-
    evaluate_polynomial(Coef, Poly1, R),
    compose_polynomials_helper(Poly1, T, RT).

% Show the natural representation of the composition
show_natural_representation([]).
show_natural_representation([(Coef, Exp) | T]) :-
    format("~d*x^~d + ", [Coef, Exp]),
    show_natural_representation(T).
