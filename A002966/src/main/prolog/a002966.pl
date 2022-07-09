inv(P/Q, Q/P).

sub(A/B, C/D, P/Q) :-
	PP is A * D - B * C, QQ is B * D,
	GG is gcd(PP, QQ),
	P is PP / GG, Q is QQ / GG.

mul(A/B, C/D, P/Q) :-
	PP is A * C, QQ is B * D,
	GG is gcd(PP, QQ),
	P is PP / GG, Q is QQ / GG.	

x_min(S, Lin, Xmin) :-
	inv(S, SS),
	(
		(SS = P / 1)
		->
		(Xmin1 is (P + 1))
		;
		(Xmin1 is ceiling(SS))
	),
	length(Lin, Z),
	(
		(Z = 0)
	->
		Xmin = Xmin1
	;
		(
		nth1(1, Lin, 1 / Xmin2),
		Xmin is max(Xmin1, Xmin2)
		)
	)
	.

x_max(N, S, Xmax) :-
	inv(S, I),
	mul(N/1, I, NI),
	Xmax is floor(NI).

% t(+N, +S, +Lin, -Lout):
% Lout is a list of N elements of the 1 / Xi form,
% less than or equal to the elements of the 1 / Xi form contained in Lin,
% and such that their sum is S.

t(N, S, Lin, Lout) :-
	N = 1, !,
	S = 1 / _,
	Lout = [S | Lin].

t(N, S, Lin, Lout) :-
	NN is N - 1, % maplist(write, ['NN=', NN, '\n']),
	x_min(S, Lin, Xmin), % maplist(write, ['Xmin=', Xmin, '\n']),
	x_max(N, S, Xmax), % maplist(write, ['Xmax=', Xmax, '\n']),
	between(Xmin, Xmax, X), % maplist(write, ['X=', X, '\n']),
	inv(X/1, Y), % maplist(write, ['Y=', Y, '\n']),
	sub(S, Y, SS), % maplist(write, ['SS=', SS, '\n']),
	t(NN, SS, [1/X | Lin], Lout).

a(N, A) :-
	aggregate_all(count, t(N, 1/1, [], _), A).
