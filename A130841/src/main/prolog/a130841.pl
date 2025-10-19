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

% ------------------------------------------------------------------------------
% Graphviz routines

graphviz_example :- % Generate a graphviz file illustrating a(12) = 
	tell('a130841.gv'),
	graphviz(oproduct(12, _)),
	told. % Remains to use Graphviz to actually convert that to a SVG.

:- dynamic counter/1.

graphviz(Goal) :-
	maplist(write, ['digraph G {\n']),
	maplist(write, ['    charset = "UTF-8";\n']),
	maplist(write, ['    rankdir = "BT";\n']),
	maplist(write, ['    node [shape = "circle" style = "filled"]\n']),
	retractall(counter(_)),
	assert(counter(0)),
	forall(Goal, (
		counter(OldCounter),
		retract(counter(OldCounter)),
		Counter is OldCounter + 1,
		assert(counter(Counter)),
		Goal =.. [_Functor, _N, Object],
		maplist(write, ['    subgraph cluster_', Counter, ' {\n']),
		maplist(write, ['        style = "filled";\n']),
		maplist(write, ['        fillcolor = "lightcyan";\n']),
		maplist(write, ['        label = "#', Counter, '";\n']),
		graphviz_object(Object),
		maplist(write, ['    }\n'])
	)),
	maplist(write, ['}\n']).

% graphviz_object(+Object)
% Draws the object Object, which may be an oterm, an oproduct.
graphviz_object(Object) :-
	graphviz_object(null, Object).

% graphviz_edge_from_parent(+Parent, +UUID)
% May draw an edge between Parent and UUID, depending on the value of Parent.
graphviz_edge_from_parent(Parent, _) :- Parent = null, !.
graphviz_edge_from_parent(Parent, UUID) :-
	maplist(write, ['        "', Parent, '" -> "', UUID, '";\n']).

% Draws the object Object and possibly also the edges from a Parent object to that Object
graphviz_object(Parent, 1) :-
	uuid(UUID),
	maplist(write, ['        "', UUID, '" [shape = "circle" fillcolor = "white" color = "black" fontcolor = "gray" label = "1"];\n']),
	graphviz_edge_from_parent(Parent, UUID).
graphviz_object(Parent, []) :-
	graphviz_object(Parent, 1). % Simplification: do not write a product when there are no factors; directly write 1.
graphviz_object(Parent, [UniqueObject]) :-
	graphviz_object(Parent, UniqueObject). % Simplification: do not write a product when there's only 1 factor; directly write that factor.
graphviz_object(Parent, Object) :-
	is_list(Object),
	length(Object, Len),
	Len > 1,
	uuid(UUID),
	maplist(write, ['        "', UUID, '" [shape = "square" fillcolor = "white" color = "black" fontcolor = "gray" label = "×"];\n']),
	graphviz_edge_from_parent(Parent, UUID),
	graphviz_list(UUID, Object).
graphviz_object(Parent, Object) :-
	Object = 1 + X,
	uuid(UUID),
	maplist(write, ['        "', UUID, '" [shape = "circle" fillcolor = "navyblue" color = "black" fontcolor = "white" label = "1+"];\n']),
	graphviz_edge_from_parent(Parent, UUID),
	graphviz_object(UUID, X).
graphviz_list(UUID, List) :-
	maplist(graphviz_object(UUID), List).
