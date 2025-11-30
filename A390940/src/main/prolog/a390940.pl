t(0, [1 - 1]).

t(J, TJ) :-
	J > 0,
	I is J - 1,
	t(I, TI),
	transpose_pairs(TI, UI),
	append(TI, UI, TIUI),
	sort(1, @=<, TIUI, TIUIs),
	group_pairs_by_key(TIUIs, KVs),
	maplist(sum_values, KVs, TJ).

sum_values(K - Values, K - SumOfValues) :-
	sum(Values, SumOfValues).

sum(List, Sum) :-
	foldl(add, List, 0, Sum).

add(X, Y, Z) :-
	Z is X + Y.
