n(S, T) :- S = x, !, T = o(x, x).
n(S, T) :- S = o(A, B), n(A, X), T = o(X, B).
n(S, T) :- S = o(A, B), n(B, X), T = o(A, X).
s(S, L) :- findall(T, n(S, T), Ts), list_to_set(Ts, L).
nmax(X) :- X is 2^1000.
cmax(X) :- X is 1000.
g(S, N) :- S = x, !, N = 0.
g(S, N) :-
	S = o(SC, SK), g(SC, C), g(SK, K),
	cmax(Cmax), nmax(Nmax),
	(C >= Cmax -> N = Nmax ; true),
	(2*K+1 >= Nmax -> N = Nmax ; true),
	(var(N) -> N is min(Nmax, (2^C)*(2*K+1)) ; true).
r(S, T) :- S = x, !, T = x.
r(S, T) :- S = o(SC, SK), r(SC, TC), r(SK, TK), T = o(TK, TC).
y(S, T) :- r(S, X), T = o(S, X).
h(S, N) :- y(S, T), g(T, N).
v(S, V - S) :- h(S, V).
e(N, E1) :- N = 1, !, E1 = [x].
e(N, EN) :-
	M is N - 1,
	e(M, EM),
	maplist(s, EM, LLM),
	flatten(LLM, LM0),
	sort(LM0, LM1),
	ord_subtract(LM1, EM, LM2),
	maplist(v, LM2, LM3),
	min_member(_ - S, LM3),
	sort([S | EM], EN).
w(X) :- maplist(write, [X, ', ']).
main(N) :-
	e(N, EN),
	maplist(h, EN, C),
	sort(C, B),
	nmax(Nmax),
	delete(B, Nmax, A),
	maplist(w, A).
bfile(N) :-
	e(N, EN),
	maplist(h, EN, C),
	sort(C, B),
	nmax(Nmax),
	delete(B, Nmax, A),
	tell('b323752.txt'),
	forall(
		nth1(I, A, AI),
		maplist(write, [I, ' ', AI, '\n'])
	),
	told.
