:- use_module(library(clpz)).

solve(_, [], _).
solve(I, [x | R], A) :-
    !,
    I2 #= I + 1,
    solve(I2, R, A).
solve(I, [N | R], A) :-
    (A + I) mod N #= 0,
    I2 #= I + 1,
    solve(I2, R, A).

answer(I, A) :-
    A #>= 100000000000000,
    A #< 10000000000000000,
    solve(0, I, A).

% To be run using scryer-prolog:
%       answer([19,x,x,x,x,x,x,x,x,x,x,x,x,37,x,x,x,x,x,883,x,x,x,x,x,x,x,23,x,x,x,x,13,x,x,x,17,x,x,x,x,x,x,x,x,x,x,x,x,x,797,x,x,x,x,x,x,x,x,x,41,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,29], A), label([A]).
%
% Other than those sketchy ranges, I think this works. It's just... too slow to produce an answer.
