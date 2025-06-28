main :-
	forall(
		between(1, infinite, N),
		(
			a(N, AN),
			maplist(write, [AN, ', ']),
			flush_output
		)
	).

a(N, AN) :-
	findall(A, solution(N, A), AS),
	list_to_set(AS, S),
%	maplist(writeln, S), % uncomment to see textual descriptions of diagrams
	length(S, AN).

solution(N, AA) :-
	est_un_appariement_connexe(N, A),
	representant(N, A, AA).

est_un_appariement_connexe(N, A) :-
	B is 2 * N - 1,
	findall(K, between(0, B, K), L),
	apparier(L, A),
	connexe(A).

apparier([], []).
apparier([H | T], [H-C | R]) :-
	select(C, T, TT),
	apparier(TT, R).

connexe([H | T]) :-
	connexe([H], T).

connexe(_, []) :- !.
connexe(Connexe, List) :-
	select(Elem1, List, List2),
	member(Elem2, Connexe),
	se_croisent(Elem1, Elem2), !,
	connexe([Elem1 | Connexe], List2).

dedans(A, B, X) :- A < X, X < B.

dehors(A, _B, X) :- X < A.
dehors(_A, B, X) :- B < X.

se_croisent(A - B, C - D) :-
	sort([A, B], [A1, B1]),
	sort([C, D], [C1, D1]),
	(
		(
			dedans(A1, B1, C1),
			dehors(A1, B1, D1)
		)
	;
		(
			dedans(A1, B1, D1),
			dehors(A1, B1, C1)
		)
	).

tourner_appariement(N, A, C, AAAA) :-
	Modulo is 2 * N,
	maplist(ajoute(Modulo, C), A, AA),
	maplist(redresse, AA, AAA),
	sort(AAA, AAAA).

redresse(X - Y, X - Y) :- X < Y.
redresse(X - Y, Y - X) :- Y < X.

ajoute(Modulo, C, X1-X2, Y1-Y2) :-
	Y1 is (X1 + C) mod Modulo,
	Y2 is (X2 + C) mod Modulo.

representant(N, A, AA) :-
	Modulo is 2 * N,
	B is Modulo - 1,
	findall(
		AShifted,
		(
			between(1, B, K),
			tourner_appariement(N, A, K, AShifted)
		),
		AShifteds),
	min_member(AA, AShifteds).

