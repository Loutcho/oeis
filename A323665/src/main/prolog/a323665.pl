v(M, V) :-
	R is mod(M, 2),
	R = 1 -> (V = 0) ; (N is M / 2, v(N, W), V is 1 + W).
a(N, S, R) :- N = 0, !, S = x, R = 0.
%a(N, S, R) :- N = 1, !, S = o(x, x), R = 1.
a(N, S, R) :-
	v(N, C),
	X is (N / 2^C),
	K is (X - 1) / 2,
	a(C, SC, RC),
	a(K, SK, RK),
	S = o(SC, SK),
	R is 1 + RC + RK.
main :- forall(between(1,87,N),(a(N,_,A),maplist(write,[A,', ']))).
bfile(Nmax) :-
	tell('b323665.txt'),
	forall(
		between(1, Nmax, N),
		(a(N,_,A), maplist(write, [N, ' ', A, '\n']))
	),
	told.

rev(S, T) :- S = x, !, T = x.
rev(S, T) :- S =
	o(SC, SK),
	T = o(SK, SC).

b(S, N) :- S = x, !, N = 0.
b(S, N) :- S = o(SC, SK),
	b(SC, C),
	b(SK, K),
	N is (2^C)*(2*K+1).

f(N, R) :- a(N, S, _), rev(S, T), b(T, R).
main2 :- forall(between(1,80,N),(f(N,F),maplist(write,[F,', ']))).

