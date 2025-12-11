:- use_module(library(clpfd)).
:- use_module(library(dcg/basics)).

lights --> ".", !, lights.
lights --> "#", !, lights.
lights --> [].

integers([N | Ns]) --> integer(N), ",", !, integers(Ns).
integers([N]) --> integer(N).

target --> "[", lights, "]".

buttons([B | Bs]) --> "(", integers(B), ") ", !, buttons(Bs).
buttons([]) --> [].

joltage(B) --> "{", integers(B), "}".

machine(machine(Joltage, Buttons)) --> target, " ", buttons(Buttons), joltage(Joltage).

machines([]) --> [].
machines([M | Ms]) --> machine(M), "\n", machines(Ms).

button_effect(I, Button, Times, Times) :- member(I, Button), !.
button_effect(_, _, _, 0).

joltage_adds_up(Buttons, Presses, I, V) :-
    length(Buttons, Len),
    length(Amounts, Len),
    sum(Amounts, #=, V),
    maplist(button_effect(I), Buttons, Presses, Amounts)
    .

max_list(N, [N]).
max_list(Max, [N | List]) :- max_list(M, List), Max is max(N, M).

min_list(N, [N]).
min_list(Min, [N | List]) :- min_list(M, List), Min is min(N, M).

flip_nth0(List, N, Nth) :- nth0(N, List, Nth).

limit_presses(Joltage, Button, Limit) :-
    maplist(flip_nth0(Joltage), Button, Targets),
    min_list(MinJoltage, Targets),
    Limit in 0..MinJoltage.

solve_in(N, Joltage, Buttons, Presses) :-
    length(Joltage, Len),
    length(Buttons, LenButtons),
    length(Presses, LenButtons),
    sum(Presses, #=, N),
    maplist(limit_presses(Joltage), Buttons, Presses),
    Max is Len - 1,
    numlist(0, Max, Indexes),
    maplist(joltage_adds_up(Buttons, Presses), Indexes, Joltage).

print_domain([]) :- write("\n").
print_domain([P | Ps]) :-
    fd_dom(P, S),
    write(S),
    write(" "),
    print_domain(Ps).

solve_machine(N, machine(Joltage, Buttons)) :-
    max_list(MinPress, Joltage),
    N #>= MinPress,
    sum(Joltage, #>=, N),
    solve_in(N, Joltage, Buttons, Presses),
    print_domain([N | Presses]),
    once(labeling([bisect, ff, min(N)], [N | Presses])),
    !,
    write(found(N)),
    write("\n").

solve_total(0, []) :- !.
solve_total(N, [Machine | Machines]) :-
    solve_machine(A, Machine),
    solve_total(B, Machines),
    N is A + B.

main :-
    read_stream_to_codes(user_input, Input),
    string_codes(Input, InputCodes),
    phrase(machines(Machines), InputCodes),
    solve_total(Total, Machines),
    write("Answer: "),
    write(Total), write("\n").
