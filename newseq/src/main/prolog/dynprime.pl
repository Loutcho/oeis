:- dynamic is_prime / 1.
:- dynamic is_composite / 2.

main(Nmax) :-
	retractall(is_prime(_)),
	retractall(is_composite(_, _)),
	assert(is_prime(2)),
	assert(is_composite(4, 2 * 2)),
	loop(3, Nmax).

loop(N, Nmax) :-
	N =< Nmax,
	(
		once(is_composite(N, _ * _))
	->
		forall(is_composite(N, X * Y), evolve(X * Y = N))
	;
		assert(is_prime(N))
	),
	aggregate_all(count, is_composite(_, _), AN),
	maplist(write, [N, ' ', AN, '\n']),
	flush_output,
	NN is N + 1,
	loop(NN, Nmax).
loop(N, Nmax) :-
	N > Nmax.

evolve(X * Y = N) :-
	retract(is_composite(N, X * Y)),
	evolve_x(X, Y),
	evolve_y(X, Y).

evolve_x(X, Y) :-
	XX is X + 1,
	(XX > Y -> true ; may_assert_is_composite(XX * Y)).

evolve_y(X, Y) :-
	YY is Y + 1,
	may_assert_is_composite(X * YY).

may_assert_is_composite(X * Y) :-
	is_composite(_, X * Y)
	->
	true
	;
	(
		N is X * Y,
		assert(is_composite(N, X * Y))
	).
