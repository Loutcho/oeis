c(X, Y) :- Y is ceil(X / 2).
f(X, Y) :- Y is floor(X / 2).
add(X, Y, Z) :- Z is X + Y.

% Drops the final 0 in a list, if any.
drop0([], []).
drop0([0], []).
drop0([X], [X]) :- not(X = 0).
drop0([X, Y | T], [X | L]) :- drop0([Y | T], L).

% Drops the initial 1's in a list, if any,
% except if the list is just [1, ..., 1] in which case we keep [1].
drop1([], []).
drop1([X], [X]).
drop1([1, Y | T], L) :- drop1([Y | T], L).
drop1([X, Y | T], [X, Y | T]) :- not(X = 1).

% Computes the simplified successor list LL of L.
% "Simplified" in the sense that we remove the initial 1's.
succ(L, LL) :-
	maplist(c, L, C),
	maplist(f, L, F),
	append(C, [0], CC),
	append([0], F, FF),
	maplist(add, CC, FF, L1),
	drop0(L1, L2),
	drop1(L2, LL).

% Computes the number A of steps needed to reach [1] by iterating succ, starting from [N].
% Side effect: outputs L and its successors until [1] is reached.
a(N, A) :- go([N], 0, A).
go(L, N, NN) :-
	writeln(L),
	L = [1] -> (NN = N) ; (succ(L, LL), N1 is N + 1, go(LL, N1, NN)).
	
graphviz(L) :-
	L = [1] -> (true) ; (succ(L, LL), maplist(write, ['"', LL, '" -> "', L, '";\n']), graphviz(LL)).
	
graphviz :-
	tell('graphviz_lines.gv'),
	forall(between(1, 16, N), graphviz([N])),
	told.