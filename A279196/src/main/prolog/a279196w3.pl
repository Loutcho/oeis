:- dynamic(memw/2).

clean :-
	retractall(memw(_, _)).

main(NMax) :-
	forall(
		between(1, NMax, N),
		(
			get_time(TStart),
			a(N, AN),
			get_time(TEnd),
			format_time(atom(End), '%Y/%m/%d %H:%M:%S ', TEnd),
			Duration is truncate(TEnd - TStart),
			maplist(write, [End, 'a(', N, ') = ', AN, ' (', Duration, 's)\n']),
			flush_output
		)
	).

a(N, AN) :- N = 1, !, AN = 1.
a(N, AN) :-
	M is N - 2,
	w([1] / M, AN).

w(_ / 0, 1) :- !.
w(Q / M, VQM) :- memw(Q / M, VQM), !.
w(Q / M, VQM) :-
	aggregate_all(sum(T), ww(Q / M, T), VQM),
	assert(memw(Q / M, VQM)).

ww(Q / M, T) :-
	s(0, Q, M, z(elision), QQ, MM),
	w(QQ / MM, T).

s(Left, [], M, z(none), QQ, MM) :-
	(
	(
	QQ = [],
	MM = M
	)
	;
	(
	Max is min(Left, M),
	between(1, Max, X),
	QQ = [X],
	MM is M - X
	)
	).

s(Left, [], M, z(elision), QQ, MM) :-
	Max is min(Left, M),
	between(1, Max, X),
	QQ = [X],
	MM is M - X.

s(Left, [], M, z(nonbreakable), QQ, MM) :-
	Max is min(Left, M),
	between(1, Max, X),
	QQ = [X],
	MM is M - X.

s(Left, [Right | Rest], M, z(elision), QQ, MM) :-
	Max is min(Left + Right, M),
	between(0, Max, X),
	(
	(X = 0)
	->
	(QQ = Y, Z = elision)
	;
	(QQ = [X | Y], Z = none)
	),
	M1 is M - X,
	s(Right, Rest, M1, z(Z), Y, MM).

s(_, [Right | Rest], M, z(none), QQ, MM) :-
	(
	(
		QQ = [],
		MM = M
	)
	;
	(
		X = 0,
		Z = nonbreakable,
		QQ = [X | Y],
		M1 is M - X,
		s(Right, Rest, M1, z(Z), Y, MM)
	)
	).

s(Left, [Right | Rest], M, z(none), QQ, MM) :-
	Max is min(Left + Right, M),
	between(1, Max, X),
	QQ = [X | Y],
	M1 is M - X,
	s(Right, Rest, M1, z(none), Y, MM).

s(Left, [Right | Rest], M, z(nonbreakable), [X | Y], MM) :-
	Max is min(Left + Right, M),
	between(0, Max, X),
	(
	(X = 0)
	->
	(Z = nonbreakable)
	;
	(Z = none)
	),
	M1 is M - X,
	s(Right, Rest, M1, z(Z), Y, MM).

ss(Q, M) :-
	forall(
		s(0, Q, M, z(elision), QQ, MM),
		maplist(write, [QQ / MM, '\n'])
	).
