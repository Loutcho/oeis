% Discrete time (n); an infinite set P of producers.
% At n = 0, each producer has an initial stock of resources equal to 0.
% At each tick n >= 1:
% - first, each producer produces 1 resource and adds it to his stock;
% - second, one chooses an (x, y) in P X P and moves x's stock to y's (this may result in a no-op if x = y).
% a(n) is the number of possible configurations of stocks after n ticks, up to a permutation of P

threaded_main :-
	thread_create(main, _Thread, [stack_limit(1 000 000 000)]).

main :-
	forall(between(0, infinite, N),
		(
			time(a(N, AN)),
			maplist(write, ['a(', N, ') = ', AN, '\n']),
			flush_output
		)
	).

:- dynamic level/2.

from_file_to_file(M / FileIn, N / FileOut) :-
	N is M + 1,
	consult(FileIn),
	tell(FileOut),
	forall(level(M, State),
		(
			%maplist(write, [debug, ' State = ', State, '\n']),
			descendants(1, State, Descendants),
			%maplist(write, [debug, ' Descendants = ', Descendants, '\n']),
			forall(member(Descendant, Descendants),
				(
					maplist(write, ['level(', N, ', ', Descendant, ').\n']),
					flush_output
				)
			)
		)
	),
	told,
	true.

a(N, AN) :-
	descendants(N, [0 - oo], Descendants),
	length(Descendants, AN).

debug_descendants(N) :-
	forall(
		between(0, N, I),
		(
			descendants(I, [0 - oo], Descendants),
			length(Descendants, AI),
			maplist(write, ['==================== a(', I, ') = ', AI, ' ====================\n']),
			maplist(writeln, Descendants)
		)
	).

descendants(0, State, [State]) :- !.
descendants(D, State, Descendants) :-
	findall(Child, evolve(State, Child), Children),
	DD is D - 1,
	maplist(descendants(DD), Children, DescendantsOfChildren),
	ord_union(DescendantsOfChildren, Descendants).

evolve(OldState, NewState) :-
	list_to_assoc(OldState, OldStateAssoc),
	setof(X,
		(
			evolve_multi(OldStateAssoc, NewStateAssoc),
			assoc_to_list(NewStateAssoc, X)
		),
		XS),
	sort(XS, XXS),
	member(NewState, XXS).

evolve_multi(OldState, NewState) :-
	produce(OldState, TmpState),
	may_move(TmpState, NewState),
	assoc_to_list(NewState, Luke),
	assoc_to_list(OldState, DarthVader),
	(Luke = [0-1,1-1,3-oo,4-1,8-1] -> writeln(father = DarthVader) ; true)
	.

produce(OldState, NewState) :-
	assoc_to_list(OldState, L),
	maplist(add1_to_key, L, LL),
	list_to_assoc(LL, NewState).

add1_to_key(K - V, KK - V) :-
	KK is K + 1.

may_move(OldState, NewState) :-
	gen_assoc(X, OldState, CX),
	gen_assoc(Y, OldState, CY),
	X =< Y,
	case_disjunction(X - CX, Y - CY, OldState, NewState).

case_disjunction(X - CX, X - CX, OldState, NewState) :-
	NewState = OldState.

case_disjunction(X - CX, X - CX, OldState, NewState) :-
	greater_than_1(CX),
	effective_move(X - CX, X - CX, OldState, NewState).

case_disjunction(X - CX, Y - CY, OldState, NewState) :-
	not(X = Y),
	effective_move(X - CX, Y - CY, OldState, NewState).

greater_than_1(oo).
greater_than_1(X) :- number(X), X > 1.

effective_move(X - CX, Y - _CY, OldState, NewState) :- % _CY as a parameter is useless and even dangerous if X = Y
	add(CX, -1, CX1),
	put_or_del_assoc(X - CX / CX1, OldState, TmpState001),
	get_assoc(Y, TmpState001, CY), % because here CY might now be distinct from _CY
	add(CY, -1, CY1),
	put_or_del_assoc(Y - CY / CY1, TmpState001, TmpState002),
	put_assoc(0, TmpState002, 1, TmpState003),
	S is X + Y,
	(get_assoc(S, TmpState003, CS) -> true ; CS = 0),
	add(CS, 1, CS1),
	put_assoc(S, TmpState003, CS1, NewState).

add(X, _Y, Z) :- X = oo, !, Z = oo.
add(_X, Y, Z) :- Y = oo, !, Z = oo.
add(X, Y, Z) :- Z is X + Y.

put_or_del_assoc(X - OldCount / NewCount, OldState, NewState) :-
	(
		NewCount = 0
	->
		del_assoc(X, OldState, OldCount, NewState)
	;
		put_assoc(X, OldState, NewCount, NewState)
	).
