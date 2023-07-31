init([0-0-1]).

degrade(P, P4) :-
	list_to_assoc(P, P0),
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
	put_assoc(I-JJ, P2, CIJJ2, P3),
	assoc_to_list(P3, P4).

elevate(P, P4) :-
	list_to_assoc(P, P0),
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
	put_assoc(I-J, P2, CIJ2, P3),
	assoc_to_list(P3, P4).

garden_of_eden(P, GE) :-
	(
		elevate(P, PP)
	->
		garden_of_eden(PP, GE)
	;
		GE = P
	).

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

descendants(P, N, Descendants) :-
	findall(PP, iterate(degrade, P, N, PP), PPL),
	list_to_set(PPL, Descendants).

a(N, AN) :-
	init(P0),
	M is N - 1,
	descendants(P0, M, Descendants),
	length(Descendants, AN).

main :-
	init(P0),
	findall(GE,
		(
			descendants(P0, 9, DS),
			member(D, DS),
			garden_of_eden(D, GE)
		),
		GEL
	),
	list_to_set(GEL, GES),
	maplist(writeln, GES).

successor(P, P1) :-	degrade(P, P1).
successor(P, P1) :-	elevate(P, P1).

