u(P, [1 | P]).

v(P, VP) :-
	reverse(P, Q),
	maplist(add, P, Q, VP).

add(X, Y, Z) :-
	ground(X),
	ground(Y), !,
	Z is X + Y.

child(P, Child) :-
	u(P, Child).
child(P, Child) :-
	v(P, Child).

descendant(D, P, Q) :-
	D = 0, !,
	Q = P.
descendant(D, P, Q) :-
	D > 0,
	child(P, PP),
	DD is D - 1,
	descendant(DD, PP, Q).

main(Filename, Depth) :-
	tell(Filename),
	forall(descendant(Depth, [1], Descendant), writeln(Descendant)),
	told.
