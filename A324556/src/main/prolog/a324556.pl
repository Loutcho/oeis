:- op(500, xfx, o).
b(N, B) :- (N = 0 -> B = -1 ; B is msb(N)).
t(N, T) :- N = 0, !, T = x.
t(N, T) :- b(N, A), P is N - (1 << A), b(P, B), R is A - B - 1,
	t(P, TP), t(R, TR), T = (TP o TR).
m(T, TT) :- T = x, !, TT = x.
m(T, TT) :- T = (TP o TR), m(TP, TTP), m(TR, TTR), TT = (TTR o TTP).
n(T, N) :- T = x, !, N = 0.
n(T, N) :- T = (TP o TR), n(TP, P), n(TR, R), b(P, B),
	A is R + B + 1, N is P + (1 << A).
a(N, A) :- t(N, T), m(T, TT), n(TT, A).
w(A) :- checklist(write, [A, ', ']), flush_output.
main :- forall(between(0, 33, N), (a(N, A), w(A))), nl.
