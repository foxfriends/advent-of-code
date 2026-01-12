:- use_module(library(clpfd)).
:- use_module(library(dcg/basics)).

var(w) --> "w".
var(x) --> "x".
var(y) --> "y".
var(z) --> "z".
var(Int) --> integer(Int).

instruction(inp(V)) --> "inp ", var(V).
instruction(add(L, R)) --> "add ", var(L), " ", var(R).
instruction(mul(L, R)) --> "mul ", var(L), " ", var(R).
instruction(div(L, R)) --> "div ", var(L), " ", var(R).
instruction(mod(L, R)) --> "mod ", var(L), " ", var(R).
instruction(eql(L, R)) --> "eql ", var(L), " ", var(R).

instructions([]) --> [].
instructions([I | Is]) --> instruction(I), "\n", instructions(Is).

val(w, [W, _, _, _], W) :- !.
val(x, [_, X, _, _], X) :- !.
val(y, [_, _, Y, _], Y) :- !.
val(z, [_, _, _, Z], Z) :- !.
val(N, _, N) :- !.

update(w, [_, X, Y, Z], [W, X, Y, Z], W).
update(x, [W, _, Y, Z], [W, X, Y, Z], X).
update(y, [W, X, _, Z], [W, X, Y, Z], Y).
update(z, [W, X, Y, _], [W, X, Y, Z], Z).

unevaluate_instruction(inp(V), Previous, Next, W) :-
    update(V, Previous, Next, W).

unevaluate_instruction(add(L, R), Previous, Next) :-
    val(L, Previous, LV),
    val(R, Previous, RV),
    NV #= LV + RV,
    update(L, Previous, Next, NV).

unevaluate_instruction(mul(L, R), Previous, Next) :-
    val(L, Previous, LV),
    val(R, Previous, RV),
    NV #= LV * RV,
    update(L, Previous, Next, NV).

unevaluate_instruction(div(L, R), Previous, Next) :-
    val(L, Previous, LV),
    val(R, Previous, RV),
    NV #= LV // RV,
    update(L, Previous, Next, NV).

unevaluate_instruction(mod(L, R), Previous, Next) :-
    val(L, Previous, LV),
    val(R, Previous, RV),
    NV #= LV mod RV,
    update(L, Previous, Next, NV).

unevaluate_instruction(eql(L, R), Previous, Next) :-
    val(L, Previous, LV),
    val(R, Previous, RV),
    LV #= RV #<==> NV,
    update(L, Previous, Next, NV).

unevaluate([], S, S, []).
unevaluate([I | Is], First, Last, Inputs) :-
    unevaluate_instruction(I, Previous, Last),
    !,
    unevaluate(Is, First, Previous, Inputs).
unevaluate([I | Is], First, Last, [Input | Inputs]) :-
    Input in 1..9,
    unevaluate_instruction(I, Previous, Last, Input),
    !,
    unevaluate(Is, First, Previous, Inputs).

evaluate(Instructions, FinalState, Inputs) :-
    reverse(Instructions, Snoitcurtsni),
    unevaluate(Snoitcurtsni, [0, 0, 0, 0], FinalState, Stupni),
    once(labeling([ff, up], Stupni)),
    reverse(Stupni, Inputs).

main :-
    read_stream_to_codes(user_input, Input),
    string_codes(Input, InputCodes),
    phrase(instructions(Instructions), InputCodes),
    evaluate(Instructions, [_, _, _, 0], Inputs),
    !,
    atomic_list_concat(Inputs, Output),
    write(Output),
    write("\n").
