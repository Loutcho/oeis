:- encoding(utf8).
:- dynamic mem/1.

main :-
	loop(1).

loop(N) :-
	a(N, AN),
	maplist(write, [AN, ', ']),
	flush_output,
	NN is N + 1,
	loop(NN).

a(N, AN) :-
	NN is 2 * N,
	t(NN, N, AN).

t(N, K, TNK) :-
	retractall(mem(_)),
	aggregate_all(count, gen(N, K, _P), TNK).

gen(N, K, R) :-
	length(P, N),
	instantiate(K, P),
	representative(P, R),
	(mem(R) -> fail ; assert(mem(R))).

instantiate(0, []).
instantiate(K, [X | T]) :-
	member(X, [0, 1]),
	J is K - X,
	instantiate(J, T).

circular_permutation(P, PP) :-
	append(P1, P2, P),
	not(P2 = []),
	append(P2, P1, PP).

representative(P, R) :-
	findall(PP, circular_permutation(P, PP), PPS),
	sort(PPS, SPPS),
	nth1(1, SPPS, R).

