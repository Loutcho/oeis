%===============================================================================
:- dynamic(f/2).
clean :-
	retractall(f(_, _)).
%===============================================================================
main(NMax) :-
	clean,
	forall(
		between(1, NMax, N),
		(
			a(N, AN),
			maplist(write, [N, ': ', AN, '\n']),
			flush_output
		)
	),
	nl.
main_time(NMax) :-
	clean,
	forall(
		between(1, NMax, N),
		(
			time(a(N, AN)),
			maplist(write, [N, ': ', AN, '\n']),
			flush_output
		)
	),
	nl.
main_profile(N) :-
	clean,
	profile(a(N, AN)),
	writeln(AN).
%===============================================================================
a(N, AN) :-
	M is N - 1,
	v([1] / M, AN).
%===============================================================================
v(_Q / M, VQM) :-
	M = 0, !, VQM = 1.
v(Q / M, VQM) :-
	f(Q / M, VQM), !.
v(Q / M, VQM) :-
	Q = [0 | QT], !,
	v(QT / M, VQM).
v(Q / M, VQM) :-
	findall(
		QQ / MM,
		valid_successor(Q / M, QQ / MM),
		QQMMs
	),
	% length(QQMMs, Len), maplist(write, ['DEBUG: Len = ', Len, '\n']),
	maplist(v, QQMMs, VQQMMs),
	foldl(plus, VQQMMs, 0, VQM),
	assert(f(Q / M, VQM)).
%===============================================================================
valid_successor(Q / R, QQ / RR) :-
	resize(Q, R, Q0),
	instanciate(Q0 / R, 0, QQ0 / RR),
	trim(QQ0, QQ),
	not(QQ = []).
%===============================================================================
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
%===============================================================================
%   Q  *   U
%     / \ /
%    *   *
%     \ /
%      *
%     /  QQ
%    *
instanciate([] / R, _U, [] / R).
instanciate([Q | Qs] / R, U, [QQ | QQs] / RRR) :-
	Max is min(R, Q + U),
	between(0, Max, QQ),
	RR is R - QQ,
	instanciate(Qs/RR, QQ, QQs/RRR).
%===============================================================================
