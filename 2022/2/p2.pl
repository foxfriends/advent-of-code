translate("A", rock).
translate("B", paper).
translate("C", scissors).

is_win(rock, scissors).
is_win(paper, rock).
is_win(scissors, paper).

shape_score(rock, 1).
shape_score(paper, 2).
shape_score(scissors, 3).

outcome_score(A, A, 3) :- !.
outcome_score(A, B, 6) :- is_win(A, B), !.
outcome_score(_, _, 0) :- !.

oppose(A, "X", B) :- is_win(A, B).
oppose(A, "Y", A).
oppose(A, "Z", B) :- is_win(B, A).

translate_game(Game, Theirs, Mine) :-
    split_string(Game, " ", " ", [A, B]),
    translate(A, Theirs),
    oppose(Theirs, B, Mine).

score(Game, N) :-
    translate_game(Game, AA, BB),
    shape_score(BB, SS),
    outcome_score(BB, AA, OS),
    N is SS + OS.

sum_score([], 0).
sum_score(["" | Games], Score) :- sum_score(Games, Score), !.
sum_score([Game | Games], AllScore) :-
    score(Game, GameScore),
    sum_score(Games, RestScore),
    AllScore is GameScore + RestScore,
    !.

main :-
    open("input", read, Fd),
    read_stream_to_codes(Fd, Contents),
    close(Fd),
    split_string(Contents, "\n", " ", Lines),
    sum_score(Lines, Score),
    write(Score), write("\n").
