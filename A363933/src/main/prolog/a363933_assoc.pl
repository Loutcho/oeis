init(P) :-
	empty_assoc(P0),
	put_assoc(0-0, P0, 1, P).

degrade(P0, P3) :-
	gen_assoc(I-J, P0, CIJ),
	CIJ2 is CIJ - 1,
	(
		CIJ2 = 0
	->
		del_assoc(I-J, P0, _, P1)
	;
		put_assoc(I-J, P0, CIJ2, P1)
	),
	II is I + 1,
	JJ is J + 1,
	(get_assoc(II-J, P1, CIIJ) -> true ; CIIJ = 0),
	(get_assoc(I-JJ, P1, CIJJ) -> true ; CIJJ = 0),
	CIIJ2 is CIIJ + 1,
	CIJJ2 is CIJJ + 1,
	put_assoc(II-J, P1, CIIJ2, P2),
	put_assoc(I-JJ, P2, CIJJ2, P3).

elevate(P0, P3) :-
	gen_assoc(II-J, P0, CIIJ),
	II > 0,
	I is II - 1,
	JJ is J + 1,
	get_assoc(I-JJ, P0, CIJJ),
	(get_assoc(I-J, P0, CIJ) -> true ; CIJ = 0),
	CIJ2 is CIJ + 1,
	CIIJ2 is CIIJ - 1,
	CIJJ2 is CIJJ - 1,
	(
		CIIJ2 = 0
	->
		del_assoc(II-J, P0, _, P1)
	;
		put_assoc(II-J, P0, CIIJ2, P1)
	),
	(
		CIJJ2 = 0
	->
		del_assoc(I-JJ, P1, _, P2)
	;
		put_assoc(I-JJ, P1, CIJJ2, P2)
	),
	put_assoc(I-J, P2, CIJ2, P3).

iterate(Pred, Input, N, Output) :-
	(
		N = 0
	->
		Output = Input
	;
		(
		Goal =.. [Pred, Input, Intermediary],
		Goal,
		M is N - 1,
		iterate(Pred, Intermediary, M, Output)
		)
	).

a(N, AN) :-
	init(P0),
	M is N - 1,
	findall(P1, iterate(degrade, P0, M, P1), P1s),
	maplist(assoc_to_list, P1s, P1s2),
	list_to_set(P1s2, P1s3),
	maplist(list_to_assoc, P1s3, P1s4),
	length(P1s4, AN).
