% A296516 - related.
% This program computes the polynomials obtained by:
% P_{0} = x
% Q_{0} = y
% P_{n + 1} = P_{n} + Q_{n}
% Q_{n + 1} = P_{n} * Q_{n}
%
% and also generates Graphviz source code to experiment
% with the syntactic trees of these polynomials.

g_iterated([X, Y], N, [XX, YY]) :-
	N = 0, !,
	XX = X, YY = Y.
g_iterated([X, Y], N, [XX, YY]) :-
	M is N - 1,
	g_iterated([X, Y], M, [X1, Y1]),
	XX = X1 + Y1,
	YY = X1 * Y1.

distribute(N, ND) :- atom(N), !,
	ND = N.
distribute(N, ND) :- N = (SAG + SAD), !,
	distribute(SAG, SAG1),
	distribute(SAD, SAD1),
	ND = SAG1 + SAD1.
distribute(N, ND) :- N = (SAG * SAD), !,
	distribute_mul(SAG, SAD, ND).
distribute(N, ND) :-
	ND = N.
not_plus(N) :-
	N =.. [F | _],
	not(F = '+').
distribute_mul(SAG, SAD, ND) :- not_plus(SAG), not_plus(SAD), !,
	distribute(SAG, SAG1),
	distribute(SAD, SAD1),
	ND = SAG1 * SAD1.
distribute_mul(SAG, SAD, ND) :- SAG = SAGSAG + SAGSAD, SAD = SADSAG + SADSAD, !,
	distribute(SAGSAG * SADSAG, GG),
	distribute(SAGSAG * SADSAD, GD),
	distribute(SAGSAD * SADSAG, DG),
	distribute(SAGSAD * SADSAD, DD),
	ND = (GG + GD) + (DG + DD).
distribute_mul(SAG, SAD, ND) :- SAG = SSAG + SSAD, not_plus(SAD), !,
	distribute(SSAG * SAD, SAG1),
	distribute(SSAD * SAD, SAD1),
	ND = SAG1 + SAD1.
distribute_mul(SAG, SAD, ND) :- not_plus(SAG), SAD = SSAG + SSAD, !,
	distribute(SAG * SSAG, SAG1),
	distribute(SAG * SSAD, SAD1),
	ND = SAG1 + SAD1.
	
graphviz1(Chose) :-
	tell('a296516.gv'),
	maplist(write, ['digraph G {\n']),
	maplist(write, ['node [shape = "circle" style = "filled"];\n']),
	graphviz2(0, Chose),
	maplist(write, ['}\n']),
	told.

fillcolor(Func, FillColor) :- Func = '+', !, FillColor = '#a6d96a'.
fillcolor(Func, FillColor) :- Func = '*', !, FillColor = '#f46d43'.
fillcolor(_, FillColor) :- FillColor = white.

get_label(Func, Label) :- Func = '+', !, Label = '&#43;'.
get_label(Func, Label) :- Func = '*', !, Label = '&#10761;'.
get_label(Func, Label) :- Label = Func.

graphviz2(Parent, Chose) :-
	Chose =.. [Func | Args],
	gensym('node', NodeName),
	fillcolor(Func, FillColor),
	get_label(Func, Label),
	maplist(write, [NodeName, '[label = "', Label, '" fillcolor = "', FillColor,'"];\n']),
	(Parent = 0 -> true ; maplist(write, [Parent, '->', NodeName, ';\n'])),
	maplist(graphviz2(NodeName), Args).

main :-
	reset_gensym(''),
	g_iterated([x, y], 4, [_P, Q]),
	checklist(write, ['Q4=', Q, '\n']),
	graphviz1(Q).
