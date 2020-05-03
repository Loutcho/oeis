%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% A way of getting A001911:
% Let x |--> x (the identity function) be the root of a graph.
% To get the children of a node, compose by one of the following:
% *    x |--> abs(x)
% *    x |--> x + 1,
% *    x |--> x - 1,
% (discard the children that yield an-already-encountered node, no cycles).
% a(n) is the number of nodes at distance n - 1 from the root.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% a Prolog program that outputs the sequence of the a(n), based on this representation:
aa(0, [x]).
aa(N, AA) :- not(N = 0), M is N - 1, aa(M, AA0), findall(S, (member(X, AA0), s(X, S)), AA).
nonnegative(abs(_)).
nonnegative(abs(_) + _).
positive(abs(_) + _).
s(X, abs(X)) :- not(nonnegative(X)).
s(x, x+1).
s(x, x-1).
s(abs(X), abs(X) + 1).
s(abs(X), abs(X) - 1).
s(X + K, X + L) :- L is K + 1.
s(X - K, X - L) :- L is K + 1.
a(N, A) :- aa(N, AA), length(AA, A).
wa(N) :- a(N, A), write(A), write(', ').
main :- forall(between(0, 20, N), wa(N)).

% additional Prolog code to generate Graphviz code (which also supports the proof, see below):
color(X, C) :-
	positive(X)
	-> C = yellow
	;
	(
		nonnegative(X)
		-> C = green
		;
		C = lightgray
	).
wb(M, _, M).
wb(M, AA, Z) :-
	not(M = Z),
	forall(
		(member(X, AA), s(X, S), color(S, C)),
		(maplist(write, ['"', S, '" [style = "filled" fillcolor = "', C, '"];\n']),
		%(maplist(write, ['"', S, '" [label = "" style = "filled" fillcolor = "', C, '"];\n']),
		maplist(write, ['"', X, '" -> "', S, '";\n']))
	),
	findall(S, (member(X, AA), s(X, S)), AA1),
	N is M + 1,
	wb(N, AA1, Z).
gvmain(N) :-
	tell('a001911.gv'),
	writeln('digraph G {'),
	writeln('rankdir = "LR";'),
	writeln('node[shape = "box"];'),
	wb(0, [x], N),
	writeln('}'),
	told.

% Sketch of a proof that this is actually A001911 that we generate this way:
%
% Let's say that a node (which is a function, f) is:
% * white  iff f is the identity
% * gray   iff f is not the identity, and f(x) can be <0, 0, or >0.
% * green  iff f(x) can only be 0 or >0;
% * yellow iff f(x) can only be >0;
% Green nodes are of the form abs(something).
%   Applying abs() to abs(something) does nothing, so a green node has no child by an abs() edge.
%   Applying x + 1 to abs(something) creates a not-already-encountered function, abs(something) + 1, so a green node always has a child by an x + 1 edge. This child happens to be yellow.
%   Applying x - 1 to abs(something) creates a not-already-encountered function, abs(something) - 1, so a green node always has a child by an x - 1 edge. This child happens to be gray.
% Gray nodes are of one of the following forms: x - something, x + something, abs(something) - something.
%   Applying abs() to a gray node creates a not-already-encountered function, which is always >=0. So a gray node always has a child by an abs() edge and this child is green.
%   Applying x + 1 to a gray node of the form x - something yields an already-encountered function, x - (something - 1) or x.
%   Applying x - 1 to a gray node of the form x - something yields a not-already-encountered function, x - (something + 1); this child happens to be gray;
%   Applying x + 1 to a gray node of the form x + something yields a not-already-encountered function, x + (something + 1); this child happens to be gray;
%   Applying x - 1 to a gray node of the form x + something yields an already-encountered function, x + (something - 1) or x;
%   Applying x + 1 to a gray node of the form abs(something) - something yields an already-encountered function, abs(something) - (something - 1) or abs(something);
%   Applying x - 1 to a gray node of the form abs(something) - something yields a not-already-encountered function, abs(something) - (something + 1); this child happens to be gray;
%   In all cases, a gray node has two children:
%      a green child,
%      a gray child.
% Yellow nodes are of the form abs(something) + something.
%   Applying abs() to abs(something) + something does nothing, so a yellow node has no child by an abs() edge.
%   Applying x + 1 to abs(something) + something yields a not-already-encountered function, abs(something) + (something + 1), so a yellow node has a child by an x + 1 edge; this child happens to be yellow.
%   Applying x - 1 to abs(something) + something yields an already-encountered function, abs(something) + (something - 1) or abs(something), so a yellow node has no child by an x - 1 edge.
% The white node (x) has three children:
%   abs(x) which is green,
%   x + 1 which is gray,
%   x - 1 which is gray.
% Let's call:
% * g(n) the number of gray nodes at distance n-1 from the root,
% * v(n) the number of green nodes at distance n-1 from the root,
% * y(n) the number of yellow nodes at distance n-1 from the root.
% Let's denote F(n) the nth Fibonacci number and proceed by induction. Our predicate is:
% P(n) =def= " a(n), the number of nodes at distance n-1 from the root, is A001911(n) = F(n + 3) - 2 decomposable as follows: g(n) = F(n + 1), v(n) = F(n), y(n) = a(n - 2)"
% P(2) is true: a(2), the number of nodes at distance 1 from the root, is 3 = F(2 + 3) - 2, decomposable as follows:
%     gray: { x + 1, x - 1 } --> g(2) = 2 = F(2 + 1)
%     green: { abs(x) }      --> v(2) = 1 = F(2)
%     yellow: { }            --> y(2) = 0 = F(0 + 3) - 2 = a(2 - 2)
%     total                      a(2) = 3
% Let's suppose that P(n) and let's prove that P(n + 1).
%     the nodes that generate gray children are the green and gray ones, so: g(n + 1) = v(n) + g(n).
%     the nodes that generate green children are the gray ones, so: v(n+1) = g(n).
%     the nodes that generate yellow children are the green and yellow ones, so: y(n + 1) = v(n) + y(n).
% Since P(n),
%     g(n + 1) = F(n) + F(n + 1)
%     v(n + 1) = F(n + 1)
%     y(n + 1) = F(n) + a(n - 2)
% Well-known is: F(n) + F(n + 1) = F(n + 2)
% Lesser known is: F(n) + A001911(n - 2) = A001911(n - 1), in which we can substitute "a" for "A001911", since P(n - 2) and P(n - 1) are true.
% Then,
%     g(n + 1) = F(n + 2)
%     v(n + 1) = F(n + 1)
%     y(n + 1) = a(n - 1)
% which is P(n + 1).
% Thus, for all n >= 2, a(n) = F(n + 3) - 2.
% As a(1) = 1 = F(1 + 3) - 2, the formula also holds for n = 1. Extend it also to n = 0 if you wish. Finally,
%
%                           a(n) = A001911(n) for all n.
%