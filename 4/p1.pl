% Logic at its finest.
is_win([
    yes(_), yes(_), yes(_), yes(_), yes(_),
    _, _, _, _, _,
    _, _, _, _, _,
    _, _, _, _, _,
    _, _, _, _, _
]) :- !.
is_win([
    _, _, _, _, _,
    yes(_), yes(_), yes(_), yes(_), yes(_),
    _, _, _, _, _,
    _, _, _, _, _,
    _, _, _, _, _
]) :- !.
is_win([
    _, _, _, _, _,
    _, _, _, _, _,
    yes(_), yes(_), yes(_), yes(_), yes(_),
    _, _, _, _, _,
    _, _, _, _, _
]) :- !.
is_win([
    _, _, _, _, _,
    _, _, _, _, _,
    _, _, _, _, _,
    yes(_), yes(_), yes(_), yes(_), yes(_),
    _, _, _, _, _
]) :- !.
is_win([
    _, _, _, _, _,
    _, _, _, _, _,
    _, _, _, _, _,
    _, _, _, _, _,
    yes(_), yes(_), yes(_), yes(_), yes(_)
]) :- !.
is_win([
    yes(_), _, _, _, _,
    yes(_), _, _, _, _,
    yes(_), _, _, _, _,
    yes(_), _, _, _, _,
    yes(_), _, _, _, _
]) :- !.
is_win([
    _, yes(_), _, _, _,
    _, yes(_), _, _, _,
    _, yes(_), _, _, _,
    _, yes(_), _, _, _,
    _, yes(_), _, _, _
]) :- !.
is_win([
    _, _, yes(_), _, _,
    _, _, yes(_), _, _,
    _, _, yes(_), _, _,
    _, _, yes(_), _, _,
    _, _, yes(_), _, _
]) :- !.
is_win([
    _, _, _, yes(_), _,
    _, _, _, yes(_), _,
    _, _, _, yes(_), _,
    _, _, _, yes(_), _,
    _, _, _, yes(_), _
]) :- !.
is_win([
    _, _, _, _, yes(_),
    _, _, _, _, yes(_),
    _, _, _, _, yes(_),
    _, _, _, _, yes(_),
    _, _, _, _, yes(_)
]) :- !.

% And here we go
convert_all([], []).
convert_all([S | Ss], [N | Ns]) :-
    number_codes(N, S),
    convert_all(Ss, Ns).

make_board([], []).
make_board([Cell | Rest], [no(Cell) | BoardRest]) :- make_board(Rest, BoardRest).

parse_boards([], []).
parse_boards(["" | Rest], Boards) :- !, parse_boards(Rest, Boards).
parse_boards([A, B, C, D, E | Rest], [Board | Boards]) :-
    split_string(A, " ", " ", AA),
    split_string(B, " ", " ", BB),
    split_string(C, " ", " ", CC),
    split_string(D, " ", " ", DD),
    split_string(E, " ", " ", EE),
    append(AA, BB, AB),
    append(AB, CC, AC),
    append(AC, DD, AD),
    append(AD, EE, BoardStr),
    convert_all(BoardStr, BoardNum),
    make_board(BoardNum, Board),
    parse_boards(Rest, Boards).

input(Calls, Boards) :-
    open("input", read, Fd),
    read_stream_to_codes(Fd, Contents),
    close(Fd),
    split_string(Contents, "\n", " ", [CallStr | BoardStr]),
    split_string(CallStr, ",", " ", CallStrs),
    convert_all(CallStrs, Calls),
    parse_boards(BoardStr, Boards).

mark(_, [], []).
mark(Call, [no(Call) | Board], [yes(Call) | Board]) :- !.
mark(Call, [C | Board], [C | Marked]) :- mark(Call, Board, Marked).

mark_all(_, [], []).
mark_all(Call, [Board | Boards], [Marked | Rest]) :-
    mark(Call, Board, Marked),
    mark_all(Call, Boards, Rest).

unmarked([], []).
unmarked([yes(_) | Rest], Unmarked) :- unmarked(Rest, Unmarked).
unmarked([no(T) | Rest], [T | Unmarked]) :- unmarked(Rest, Unmarked).

winner([_ | Calls], []-Boards, Score) :- winner(Calls, Boards-[], Score).
winner([Call | _], [Board | _]-_, Score) :-
    mark(Call, Board, Marked),
    is_win(Marked),
    !,
    unmarked(Marked, Unmarked),
    sum_list(Unmarked, Sum),
    Score is Sum * Call.
winner([Call | Calls], [Board | Boards]-Done, Score) :-
    mark(Call, Board, Marked),
    winner([Call | Calls], Boards-[Marked | Done], Score).

main :-
    input(Calls, Boards),
    winner(Calls, Boards-[], Score),
    write(Score), write("\n").

:- main.
