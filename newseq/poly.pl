%
% Let us define the set S thus:
% (i) 1 is in S
% (ii) P is in S implies 1 + x*P is in S
% (iii) ((P is in S) and (Q is in S)) implies P*Q is in S
% (iv) if the repeated application of (i), (ii), (iii) fails to prove
%      that P is in S then P is not in S.
%
% Let us define a(n) as the number of P in S such that P(1) = n.
%

:- dynamic is_in_S/3.
:- dynamic deferred_computation/2.

main(Nmax) :-
	init,
	compute(Nmax),
	export_data('data.txt', 1, Nmax),
	export_bfile('bfile.txt', 1, Nmax).

init :-
	retractall(is_in_S(_, _, _)),
	retractall(deferred_computation(_, _)).

compute(Nmax) :-
	assert_is_in_S([1,1], Nmax, 2),
	N = 3,
	process_deferred_computations_up_to(Nmax, N).

assert_is_in_S(P, Nmax, Pedigree) :-
	is_in_S(P, _PP1, AnotherPedigree) % Guard against already known results
	->
		maplist(write, [Pedigree , ' = ', P, ' comes to the same as: ', AnotherPedigree, ' = ', P, '\n'])
	;
	(
		evaluation_at_1(P, P1),
		assert(is_in_S(P, P1, Pedigree)),
		E1 is P1 + 1,
		(E1 =< Nmax -> assert(deferred_computation(sigma(P), E1)) ; true),
		forall(is_in_S(Q, Q1, _), (
			E2 is P1 * Q1,
			(E2 =< Nmax -> assert(deferred_computation(Q * P, E2)) ; true)
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
	process_deferred_computations_of_the_sigma_type(N, Nmax),
	process_deferred_computations_of_the_product_type(N, Nmax),
	a(N, AN),
	maplist(write, ['a(', N, ') = ', AN, '\n']).

process_deferred_computations_of_the_sigma_type(N, Nmax) :-
	aggregate_all(count, deferred_computation(sigma(_), N), CountSigma),
	maplist(write, ['sigma(P):', CountSigma, '\n']),
	forall(deferred_computation(sigma(P), N),
	(
		successor(P, Q),
		is_in_S(P, _, PedigreeP),
		add_1_to_pedigree(PedigreeP, PedigreeQ),
		assert_is_in_S(Q, Nmax, PedigreeQ)
	)),
	retractall(deferred_computation(sigma(_), N)).

add_1_to_pedigree(N, NN) :-
	number(N),
	NN is N + 1.
add_1_to_pedigree(N + Something, NN + Something) :-
	number(N),
	NN is N + 1.
add_1_to_pedigree(P * Q, 1 + P * Q).

process_deferred_computations_of_the_product_type(N, Nmax) :-
	aggregate_all(count, deferred_computation(_ * _, N), CountProduct),
	maplist(write, ['P*Q: ', CountProduct, '\n']),
	forall(deferred_computation(P * Q, N),
	(
		product(P, Q, PQ),
		is_in_S(P, _, PedigreeP),
		is_in_S(Q, _, PedigreeQ),
		assert_is_in_S(PQ, Nmax, PedigreeP * PedigreeQ)
	)),
	retractall(deferred_computation(_ * _, N)).

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
	aggregate_all(count, is_in_S(_, N, _), AN).

delta_a(N, BN) :-
	a(N, AN),
	M is N - 1,
	a(M, AM),
	BN is AN - AM.

% 18 is the first N such that a(N) is not equal to A130841(N). Why?
of_interest_18 :-
	 tell('18.txt'),
	 forall(
		is_in_S(P, 18, Pedigree),
		writeln(P = Pedigree)
	),
	told.
