:- dynamic(memv/2).

clean :-
	retractall(memv(_, _)).

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

a(N, AN) :-
	M is N - 1,
	v([1] / M, AN).

nbnz([], 0).
nbnz([H | T], N) :-
	(H = 0 -> I = 0 ; I = 1),
	nbnz(T, NN),
	N is NN + I.

v(_ / 0, 1) :- !.
v(Q / 1, VQM) :- !, nbnz(Q, VQM).
v(Q / M, VQM) :- memv(Q / M, VQM), !.
v(Q / M, VQM) :- Q = [0 | QT], !, v(QT / M, VQM).
v(Q / M, VQM) :-
	findall(
		QQ / MM,
		valid_successor(Q / M, QQ / MM),
		QQMMs
	),
	maplist(v, QQMMs, VQQMMs),
	foldl(plus, VQQMMs, 0, VQM),
	assert(memv(Q / M, VQM)),
	get_time(T),
	format_time(atom(Timestamp), '%Y/%m/%d %H:%M:%S ', T),
	maplist(write, [Timestamp, memv(Q / M, VQM), '.\n']).

valid_successor(Q / R, QQ / RR) :-
	resize(Q, R, Q0),
	instanciate(Q0 / R, 0, QQ0 / RR),
	trim(QQ0, QQ),
	not(QQ = []).

repl(X, N, L) :-
    length(L, N),
    maplist(=(X), L).

resize(Q, N, QQ) :-
	M is max(0, N - 1),
	repl(0, M, L),
	append(Q, L, QQ).

trim_left([], []).
trim_left(X, Y) :- X = [0 | XX], trim_left(XX, Y).
trim_left(X, Y) :- X = [H | _], not(H = 0), Y = X.

trim(Q0, Q4) :- trim_left(Q0, Q1), reverse(Q1, Q2), trim_left(Q2, Q3), reverse(Q3, Q4).

instanciate([] / R, _U, [] / R).
instanciate([Q | Qs] / R, U, [QQ | QQs] / RRR) :-
	Max is min(R, Q + U),
	between(0, Max, QQ),
	RR is R - QQ,
	instanciate(Qs/RR, QQ, QQs/RRR).
