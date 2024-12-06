% Discrete time (k); n producers.
% Each producer has an initial (k = 0) stock of resources equal to 0.
% At each tick k >= 1:
% - first, each producer produces 1 resource and adds it to his stock;
% - second, one has the ability to move one producer's stock to another's.
% T(n,k) is the number of possible configurations of stocks after k ticks, up to a permutation.
%
%
% Used for comparison with the infinite case which is the subject of A378734.
% (one must find the same results for T(n, k) and a(k) when n gets sufficiently bigger than k)
%

initial_state(N, State) :-
	length(State, N),
	maplist('='(0), State). % State = [0, 0, ..., 0]

add(X, Y, XPlusY) :-
	XPlusY is X + Y.

evolution(StateBefore, StateAfter) :-
	production(StateBefore, TmpState),
	potential_displacement(TmpState, StateAfter).

production(StateBefore, StateAfter) :-
	maplist(add(1), StateBefore, StateAfter).

potential_displacement(State, State). % it is legal to do nothing.
potential_displacement(StateBefore, StateAfter) :-
	move_some_stock(StateBefore, StateAfter).

move_some_stock_multi(StateBefore, StateAfter) :-
%	(StateBefore = [1, 3, 3, 3, 3, 4, 4] -> Debug = 1 ; Debug = 0),
%	(Debug = 1 -> writeln(stateBefore = StateBefore) ; true),
	append(L1, L23, StateBefore),
%	(Debug = 1 -> writeln('===========================') ; true),
%	(Debug = 1 -> writeln(l1 = L1) ; true),
%	(Debug = 1 -> writeln(l23 = L23) ; true),
	append([X | R2], [Y | R3], L23),
%	(Debug = 1 -> writeln(x = X) ; true),
%	(Debug = 1 -> writeln(r2 = R2) ; true),
%	(Debug = 1 -> writeln(y = Y) ; true),
%	(Debug = 1 -> writeln(r3 = R3) ; true),
	S is X + Y,
%	(Debug = 1 -> writeln(s = S) ; true),
	append([0 | R2], [S | R3], LL23),
%	(Debug = 1 -> writeln(ll23 = LL23) ; true),
	append(L1, LL23, LL),
%	(Debug = 1 -> writeln(ll = LL) ; true),
	sort(0, @=<, LL, StateAfter).
%	(Debug = 1 -> writeln(stateAfter = StateAfter) ; true),
%	(Debug = 1 -> writeln('------------------') ; true).

% gets rid of the multiplicities of the solutions that move_some_stock_multi provides
move_some_stock(StateBefore, StateAfter) :-
	setof(X, move_some_stock_multi(StateBefore, X), XS),
	member(StateAfter, XS).

% bug avec:
% ?- move_some_stock([1, 3, 3, 3, 3, 4, 4], StateAfter).
% StateAfter = [0, 1, 3, 3, 3, 3, 8] ;
% StateAfter = [0, 1, 3, 3, 3, 4, 7] ;
% StateAfter = [0, 1, 3, 3, 4, 4, 6] ;
% StateAfter = [0, 3, 3, 3, 3, 4, 5] ;
% StateAfter = [0, 3, 3, 3, 4, 4, 4].
%     MANQUE : [0, 1, 3, 3, 3, 4, 8] <========= MANQUE

depth_evolution_multi(K, InitialState, FinalState) :-
	K = 0, !,
	FinalState = InitialState.

depth_evolution_multi(K, InitialState, FinalState) :-
	K > 0,
	J is K - 1,
	evolution(InitialState, IntermediaryState),
	depth_evolution(J, IntermediaryState, FinalState).

depth_evolution(K, InitialState, FinalState) :-
	setof(X, depth_evolution_multi(K, InitialState, X), XS),
	member(FinalState, XS).

configurations(N, K, FinalStates) :-
	initial_state(N, InitialState),
	findall(FinalState, depth_evolution(K, InitialState, FinalState), FinalStates).

t(N, K, TNK) :-
	configurations(N, K, FinalStates),
	length(FinalStates, TNK).

main :-
	forall(between(0, 30, N),
		(forall(between(0, 7, K),
			(
				t(N, K, T),
				maplist(write, [T, ', ']),
				flush_output
			)
		),
		nl, flush_output)
	).

