t(0, [1 - 1]).

t(J, TJ) :-
	J > 0,
	I is J - 1,
	t(I, TI),
	transpose_pairs(TI, UI),
	append(TI, UI, TIUI),
	sort(1, @=<, TIUI, TIUIs),
	group_pairs_by_key(TIUIs, KVs),
	maplist(sum_values, KVs, TJ),
	graphviz_cluster(J, TI, TJ).

sum_values(K - Values, K - SumOfValues) :-
	sum(Values, SumOfValues).

sum(List, Sum) :-
	foldl(add, List, 0, Sum).

add(X, Y, Z) :-
	Z is X + Y.

diffs(J) :-
	t(J, TJ),
	I is J - 1,
	t(I, TI),
	ord_subtract(TI, TJ, C),
	ord_subtract(TJ, TI, D),
	writeln('------------------------' - disappear),
	maplist(writeln, C),
	writeln('------------------------' - appear),
	maplist(writeln, D).

rgb_color(N, R, G, B) :-
	R is (N * 7 + 3) mod 255,
	G is (N * 13 + 5) mod 255,
	B is (N * 17 + 8) mod 255.

graphviz_cluster(J, TI, TJ) :-
	maplist(write, ['subgraph cluster_', J, '{\n']),
	maplist(write, ['    label = "j = ', J, '";\n']),
	forall(
		member(K - VJ, TJ),
		graphviz_node(J, K, VJ, TI)
	),
	maplist(write, ['}\n']),
	I is J - 1,
	forall(
		member(K - VJ, TJ),
		graphviz_edge(J, K, VJ, I, TI)
	).

node_name(J, K, V, Name) :-
	atomic_list_concat([J, ': T[', K, '] = ', V], Name).

node_color(VI, VJ, '#777777') :-
	VI = VJ.
node_color(VI, VJ, '#0077FF') :-
	not(VI = VJ).

graphviz_node(J, K, VJ, TI) :-
	node_name(J, K, VJ, NameJ),
	(
		member(K - VI, TI)
	->
		node_color(VI, VJ, Color)
	;
		Color = '#00FF00'
	),
	maplist(write, ['    "', NameJ, '" [fillcolor = "', Color, '"];\n']).

graphviz_edge(J, K, VJ, I, TI) :-
	member(K - VI, TI)
	->
	(
		node_name(I, K, VI, NameI),
		node_name(J, K, VJ, NameJ),
		maplist(write, ['"', NameI, '" -> "', NameJ, '";\n'])
	)
	;
	true.

main :-
	tell('toto.gv'),
	maplist(write, ['digraph G {\n']),
	maplist(write, ['    rankdir = "LR";\n']),
	maplist(write, ['    node [shape = "box" style = "filled"];\n']),
	t(80, _T),
	maplist(write, ['}\n']),
	told.
