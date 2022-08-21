main :- iter(1).
iter(K) :-
	(legal(K * x ^ 0) -> (maplist(write, [K, ', ']), flush_output) ; true),
	KK is K + 1, iter(KK).
legal(1 * x ^ 0).
legal(K * x ^ N) :-
	NN is N + 1, 0 is K mod NN, KK is K / NN,
	legal(KK * x ^ NN).
legal(K * x ^ N) :-
	((K = 1, N = 1) ; (N > 1)), NN is N - 1,
	legal(K * x ^ NN).
