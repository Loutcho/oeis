% ------------------------------------------------------------------------------
% Main entry points to display the results

% main(+Nmax)
% Prints the terms of the sequence from 1 to Nmax, in "a(N) = AN\n"* format.
main(Nmax) :-
	forall(between(1, Nmax, N), (
		a(N, AN),
		maplist(write, ['a(', N, ') = ', AN, '\n']),
		flush_output
	)).

% data(+Nmax)
% Prints the terms of the sequence from 1 to Nmax, in "AN, "* (data) format.
data(Nmax) :-
	forall(between(1, Nmax, N), (
		a(N, AN),
		maplist(write, [AN, ', ']),
		flush_output
	)).

% bfile(+Nmax)
% Prints the terms of the sequence from 1 to Nmax, in "N AN\n"* (bfile) format.
bfile(Nmax) :-
	forall(between(1, Nmax, N), (
		a(N, AN),
		maplist(write, [N, ' ', AN, '\n']),
		flush_output
	)).

% ------------------------------------------------------------------------------
% Computations

% a(+N, -AN)
% AN is a(N).
a(N, AN) :-
	aggregate_all(count, oproduct(N, _), AN).

% oproduct(+Size, -OProduct)
% OProduct is a list of oterms sorted in increasing order of their sizes
% and such that the product of their sizes equals Size.
oproduct(1, []).
oproduct(N, [H | T]) :-
	oproduct(N, 1, [H | T]).

% oproduct(+Size, +MinSize, -OProduct)
% OProduct is a list of oterms sorted in increasing order of their sizes
% and such that the product of their sizes equals Size.
% Moreover, every oterm in OProduct has a size at least equal to MinSize.
oproduct(1, _, []).
oproduct(N, MinSize, [H | T]) :-
	strict_divisor(N, D),
	D >= MinSize,
	ND is N / D,
	oterm(D, H),
	oproduct(ND, D, T).

% oterm(+Size, -Oterm)
% Oterm is an oterm with size Size
oterm(N, O) :-
	N > 1,
	M is N - 1,
	oproduct(M, P),
	O = 1 + P.

% strict_divisor(+N, -D)
% D is a strict divisor of N.
strict_divisor(N, D) :-
	between(2, N, D),
	0 is N mod D.

% ------------------------------------------------------------------------------
% Debugging routines

% debug_oproducts(+N)
% Prints all the oproducts with size N
debug_oproducts(N) :-
	forall(oproduct(N, OProduct),
		writeln(OProduct)
	),
	aggregate_all(count, oproduct(N, _), X),
	maplist(write, ['Number: ', X, '\n']).

% debug_oterms(+N)
% Prints all the oterms with size N
debug_oterms(N) :-
	forall(oterm(N, OTerm),
		writeln(OTerm)
	),
	aggregate_all(count, oterm(N, _), X),
	maplist(write, ['Number: ', X, '\n']).
