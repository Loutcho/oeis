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
	maplist(write, ['    subgraph cluster_', J, '{\n']),
	maplist(write, ['        label = "j = ', J, '";\n']),
	forall(
		member(K - VJ, TJ),
		graphviz_node(J, K, VJ, TI)
	),
	maplist(write, ['    }\n']),
	I is J - 1,
	forall(
		member(K - VJ, TJ),
		graphviz_edge(J, K, VJ, I, TI)
	).

node_name(J, K, V, Name) :-
	factor(K, AK, EK),
	factor(V, AV, EV),
	atomic_list_concat([J, ': T[', AK, '*2^', EK, '] = ', AV, '*2^', EV], Name).

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
	maplist(write, ['        "', NameJ, '" [fillcolor = "', Color, '"];\n']).

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

% N equals A * 2 ^ E
factor(N, A, E) :-
	factor(N, 0, A, E).
	
factor(N, E0, A, E) :-
	0 is N mod 2
	->
	(
		N0 is N / 2,
		E1 is E0 + 1,
		factor(N0, E1, A, E)
	)
	;
	(
		N = A,
		E = E0
	).

modelisation_80([
	aliv(         1 * 2^interval( 8 - 39),  1 * 2^1),
	dead(         9 * 2^interval( 3 -  3), 15 * 2^7),
	aliv(        17 * 2^interval( 5 - 57), 33 * 2^2),
	aliv(        25 * 2^interval( 2 - 64),  3 * 2^3),
	aliv(        45 * 2^interval( 1 - 30),  1 * 2^6),
	dead(        79 * 2^interval( 3 -  6),  1 * 2^7),
	dead(        87 * 2^interval( 7 -  9), 15 * 2^7),
	dead(        91 * 2^interval( 5 -  6), 11 * 2^3),
	dead(        97 * 2^interval( 2 -  2),  1 * 2^6),
	aliv(       111 * 2^interval( 2 - 27), 67 * 2^2),
	aliv(       235 * 2^interval( 3 - 60), 15 * 2^1),
	aliv(       319 * 2^interval( 8 - 47), 15 * 2^7),
	aliv(       737 * 2^interval( 2 - 49), 13 * 2^5),
	aliv(       773 * 2^interval( 6 - 56), 11 * 2^3),
	aliv(       961 * 2^interval( 1 - 22), 29 * 2^8),
	dead(       985 * 2^interval( 4 -  5), 67 * 2^2),
	aliv(      2421 * 2^interval( 1 - 63),  1 * 2^2),
	aliv(      2495 * 2^interval( 2 - 54),  1 * 2^7),
	dead(      8647 * 2^interval( 3 -  4), 29 * 2^8),
	dead(      8657 * 2^interval( 7 -  8), 67 * 2^2),
	dead(      9043 * 2^interval( 7 -  8),  1 * 2^6),
	aliv(    359901 * 2^interval( 2 - 51),  7 * 2^2),
	dead(   8978321 * 2^interval(17 - 18), 67 * 2^2),
	dead(   9306067 * 2^interval(17 - 18),  1 * 2^6),
	dead(   9837631 * 2^interval(13 - 14), 29 * 2^8),
	dead(  78707775 * 2^interval(16 - 17), 29 * 2^8),
	dead( 981069359 * 2^interval(32 - 33),  1 * 2^1),
	dead(7848554879 * 2^interval(35 - 36),  1 * 2^1),
	dead(9529458643 * 2^interval(27 - 28),  1 * 2^6)
]).

check(Modelisation) :-
	tell(fichier),
	generate(Modelisation),
	told.

generate(Modelisation) :-
	maplist(generate_element, Modelisation).
generate_element(aliv(A*2^interval(Min - Max), AA*2^BB)) :-
	!,
	generate_all(A, Min, Max, AA*2^BB).
generate_element(dead(A*2^interval(Min - Max), AA*2^BB)) :-
	!,
	generate_all(A, Min, Max, AA*2^BB).
generate_element(_).

generate_all(A, Min, Max, AA*2^BB) :-
	forall(between(Min, Max, N),
		maplist(write, ['t80(', A*2^N, ', ', AA*2^BB, ').\n'])
	).
