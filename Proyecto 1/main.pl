% The result of executing this program should be:
%
% zero(x)     = 0
% p(x)        = 4x^3 + 3x^2 + 2x + 1
% q(x)        = 3x^2 + 5
% p(x) + q(x) = 4x^3 + 6x^2 + 2x + 6
% p(x) * q(x) = 12x^5 + 9x^4 + 26x^3 + 18x^2 + 10x + 5
% p(q(x))     = 108x^6 + 567x^4 + 996x^2 + 586
% 0 - p(x)    = -4x^3 - 3x^2 - 2x - 1
% p(3)        = 142
% p'(x)       = 12x^2 + 6x + 2
% p''(x)      = 24x + 6


% Calculates preset examples and prints them
main:-
	p_ini(2, 3),
	q_ini(4, 5),

	% write results
	write("zero(x)     = "), zero(Z), to_string(Z), nl,
	write("p(x)        = "), p(P), to_string(P), nl,
	write("q(x)        = "), q(Q), to_string(Q), nl,
        write("p(x) + q(x) = ")
        write("p(x) * q(x) = ")
        write("p(q(x))     = ")
        write("0 - p(x)    = ")
        write("p(3)        = ")
        write("p'(x)       = ")
        write("p''(x)      = ")

