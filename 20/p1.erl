-module(p1).
-export([main/0]).

shift(_, 0, L, R) -> lists:reverse(L) ++ R;
shift(X, N, [], R) when N < 0 -> lists:reverse(shift(X, -N, [], lists:reverse(R)));
shift(X, N, [], R) when N >= length(R) - 1 -> shift(X, N rem (length(R) - 1), [], R);
shift(X, N, L, [X]) -> shift(X, N, [], [X | lists:reverse(L)]);
shift(X, N, L, [X, Y | R]) -> shift(X, N - 1, [Y | L], [X | R]);
shift(X, N, L, [Y | R]) -> shift(X, N, [Y | L], R).

mix([], L) -> L;
mix([{_,N} = X | Xs], L) -> mix(Xs, shift(X, N, [], L)).

answer(1000 = N, L, [{_,X}=E | R]) -> X + answer(N + 1, [E | L], R);
answer(2000 = N, L, [{_,X}=E | R]) -> X + answer(N + 1, [E | L], R);
answer(3000, _, [{_,X} | _]) -> X;
answer(N, L, [X | R]) -> answer(N + 1, [X | L], R);
answer(N, L, []) -> answer(N, [], lists:reverse(L)).

answer(L, [{_,0}=X | R]) -> answer(0, [], [X | R] ++ lists:reverse(L));
answer(L, [X | R]) -> answer([X | L], R).

answer(L) -> answer([], L).

read_to_eof() ->
    case io:fread("", "~d") of
        {ok, [D]} -> [D | read_to_eof()];
        eof -> []
    end.

main() ->
    Encoded = read_to_eof(),
    Decoded = mix(lists:enumerate(Encoded), lists:enumerate(Encoded)),
    io:fwrite("~B~n", [answer(Decoded)]),
    init:stop().
