:- dynamic(memorized/1).

main(Z) :- forall(between(1, Z, N),
	(a(N, A), maplist(write, [A, ', ']), flush_output)).

a(N, A) :- abolish(memorized, 1), assertz(memorized([])),
	forall(solution(N, S), memorize(S)), memorized(L), length(L, A).

memorize(S) :- memorized(L), (ord_memberchk(S, L) -> true ;
	(abolish(memorized, 1), ord_union(L, [S], LL), assertz(memorized(LL)))).

g(S, Y) :- f(S, X), length(S, M), Y is M + 2 + X.

f([], 0). f([H | T], R) :- length([H | T], L), X is L * H, f(T, S), R is S + X.

solution(N, G) :- M is N - 1, length(S, M), A is M * N, solve(M, S, A), g(S, G).

solve(M, [], 0) :- M = 0, !.
solve(M, [Q], A) :- M = 1, !, Q is A div 2.
solve(M, S, A) :- S = [H | T], B is M * (M + 1), Q is A div B,
	between(0, Q, H), AA is A - B * H, L is M - 1, solve(L, T, AA).

