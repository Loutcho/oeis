:- dynamic is_in_S/2.
:- dynamic deferred_computation/2.

main(Nmax) :-
	init,
	assert_is_in_S([1,1], Nmax),
	N = 3,
	process_deferred_computations_up_to(Nmax, N),
	export_data('data.txt', 1, Nmax),
	export_bfile('bfile.txt', 1, Nmax).

init :-
	retractall(is_in_S(_, _)),
	retractall(deferred_computation(_, _)).

assert_is_in_S(P, Nmax) :-
	is_in_S(P, PP1) % Guard against already known results
	->
		maplist(write, ['Already known: ', P, ' with a P1 = ', PP1, ' is in S\n'])
	;
	(
		evaluation_at_1(P, P1),
		assert(is_in_S(P, P1)),
		E1 is P1 + 1,
		(E1 =< Nmax -> assert(deferred_computation(sigma(P), E1)) ; true),
		forall(is_in_S(Q, Q1), (
			E2 is P1 * Q1,
			(E2 =< Nmax -> assert(deferred_computation(P * Q, E2)) ; true)
		))
	).

evaluation_at_1(P, E) :-
	sum(P, E).

sum([], 0).
sum([H | T], Sum) :-
	sum(T, Sum0),
	Sum is Sum0 + H.

process_deferred_computations_up_to(Nmax, N) :-
	N =< Nmax,
	maplist(write, ['---------------------- N = ', N, ' ------------------------\n']),
	process_deferred_computations(N, Nmax),
	NN is N + 1,
	process_deferred_computations_up_to(Nmax, NN).
process_deferred_computations_up_to(Nmax, N) :-
	N > Nmax.

process_deferred_computations(N, Nmax) :-
	aggregate_all(count, deferred_computation(sigma(_), N), CountSigma),
	maplist(write, ['sigma(P):', CountSigma, '\n']),
	forall(deferred_computation(sigma(P), N),
	(
		retract(deferred_computation(sigma(P), N)),
		% maplist(write, ['Processing: ', sigma(P), '\n']),
		successor(P, Q),
		(is_in_S(Q, _Q1) -> maplist(write, ['Unbelievable, ', Q, ' is already in S!\n']) ; assert_is_in_S(Q, Nmax))
	)),
	aggregate_all(count, deferred_computation(_ * _, N), CountProduct),
	maplist(write, ['P*Q: ', CountProduct, '\n']),
	forall(deferred_computation(P * Q, N),
	(
		retract(deferred_computation(P * Q, N)),
		product(P, Q, PQ),
		assert_is_in_S(PQ, Nmax)
	)),
	a(N, AN),
	maplist(write, ['a(', N, ') = ', AN, '\n']).
 
successor(P, [1 | P]).
 
product(P, Q, PQ) :-
	ground(P),
	ground(Q),
	length(P, LP),
	DP is LP - 1,
	length(Q, LQ),
	DQ is LQ - 1,
	DPQ is DP + DQ,
	findall(K, between(0, DPQ, K), KS),
	maplist(compute_coefficient(P, DP, Q, DQ), KS, PQ).
 
compute_coefficient(P, DP, Q, DQ, K, CK) :-
	findall(AI * BJ,
		(
			between(0, DP, I),
			J is K - I,
			between(0, DQ, J),
			nth0(I, P, AI),
			nth0(J, Q, BJ)
		),
		AIBJ
	),
	sum(AIBJ, CK).

export_data(Filename, Nmin, Nmax) :-
	tell(Filename),
	forall(between(Nmin, Nmax, N),
		(
			a(N, AN),
			maplist(write, [AN, ', '])
		)
	),
	nl,
	told.
 
export_bfile(Filename, Nmin, Nmax) :-
	tell(Filename),
	forall(between(Nmin, Nmax, N),
		(
			a(N, AN),
			maplist(write, [N, ' ', AN, '\n'])
		)
	),
	told.

a(1, 1).
a(N, AN) :-
	N > 1,
	aggregate_all(count, is_in_S(_, N), AN).
