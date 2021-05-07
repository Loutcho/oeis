% Cf. A343952.

main :-	forall(between(1, 7, N), (a(N, AN), maplist(write, [N, ' : ', AN, '\n']))).

a(N, A) :- x_ancestry(N, N, A).

x_ancestry(0, N, A) :- N = 1, !, A = 1.
x_ancestry(0, _N, 0).
x_ancestry(1, N, A) :- N = 1, !, A = 2.
x_ancestry(1, _N, 0).
x_ancestry(I, N, A) :-
	I >= 2,
	H is I - 1,
	x_ancestry(H, N, AX),
	y_ancestry(H, N, AY),
	x_combine(AX, AY, A).

x_combine(0, 0, 0).
x_combine(NZ, 0, NZ) :- not(NZ = 0).
x_combine(0, NZ, NZ) :- not(NZ = 0).
x_combine(NZ1, NZ2, NZ1 + NZ2) :- not(NZ1 = 0), not(NZ2 = 0).

y_ancestry(0, N, A) :- N = 1, !, A = 1.
y_ancestry(0, _N, 0).
y_ancestry(1, N, A) :- N = 2, !, A = 1.
y_ancestry(1, _N, 0).
y_ancestry(I, N, A) :-
	I >= 2, H is I - 1, M is N - 1,
	findall(U + V, (
			between(1, M, U),
			V is N - U,
			nonzero_x_coef(H, U),
			nonzero_y_coef(H, V)
		), UV),
	maplist(ancestry(H), UV, AU_AV),
	foldl(plus, AU_AV, 0, A).

ancestry(H, U + V, AU * AV) :- x_ancestry(H, U, AU), y_ancestry(H, V, AV).

plus(X, Y, S) :- Y = 0, !, S = X.
plus(X, Y, X + Y).

:-dynamic(memoized_fibonacci/2).
fibonacci(N, FN) :- N = 0, !, FN = 0.
fibonacci(N, FN) :- N = 1, !, FN = 1.
fibonacci(N, FN) :-
	N >= 2,
	memoized_fibonacci(N, FN) -> true ;
	(
		L is N - 2,
		fibonacci(L, FL),
		M is N - 1,
		fibonacci(M, FM),
		FN is FL + FM,
		assertz(memoized_fibonacci(N, FN))
	).

nonzero_x_coef(I, N) :- J is I + 1, fibonacci(J, FJ), between(1, FJ, N).
nonzero_y_coef(I, N) :- J is I + 1, K is I + 2, fibonacci(K, FK), between(J, FK, N).

%?- main.
%1 : 2
%2 : 1
%3 : 2*1
%4 : 2*(2*1)
%5 : 1*(2*1)+2*(2*(2*1))
%6 : 1*(2*(2*1))+2*(1*(2*1))+2*(2*(2*(2*1)))
%7 : (2*1)*(2*(2*1))+1*(1*(2*1))+(1*(2*(2*(2*1)))+2*(1*(2*(2*1))+2*(1*(2*1))))+2*(2*(2*(2*(2*1))))

gv(X) :-
	tell('C:\\Users\\Luc\\Desktop\\ancestry.gv'),
	writeln('digraph G {'),
	sub_gv('/', X),
	writeln('}'),
	told.
sub_gv(IdParent, X) :-
	X =.. [F | Args],
	uuid(IdX),
	shape(F, S),
	color(F, C),
	maplist(write, ['\t"', IdX, '" [label = "', F, '" shape = "', S, '" style = "filled" fillcolor = "', C, '"];\n']),
	maplist(write, ['\t"', IdParent, '" -> "', IdX, '";\n']),
	maplist(sub_gv(IdX), Args).
shape(N, triangle) :- member(N, ['+', '*', '/']).
shape(N, circle) :- member(N, [1, 2]).
color('*', tomato).
color('+', yellow).
color(2, beige).
color(1, lightblue).
color('/', blue).
color(X, 'grey') :- not(member(X, ['*', '+', '/', 1, 2])).