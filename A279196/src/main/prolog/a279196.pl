%===============================================================================
:- dynamic(dynamic_count/2).
:- dynamic(memoized_v/3).
:- dynamic(sum/3).
clean :-
	retractall(dynamic_count(_, _)),
	retractall(memoized_v(_, _, _)),
	retractall(sum(_, _, _)).
count_successes(Goal, Key, Successes) :-
	retractall(dynamic_count(Key, _)),
	assert(dynamic_count(Key, 0)),
	forall(Goal, (
		dynamic_count(Key, N),
		retract(dynamic_count(Key, N)),
		NN is N + 1,
		assert(dynamic_count(Key, NN))
	)),
	dynamic_count(Key, Successes).
%===============================================================================
a(N, AN) :-
	M is N - 1,
	v([1], M, AN).
%===============================================================================
v(_Q, M, VQM) :-
	M = 0, !, VQM = 1.
v(Q, M, VQM) :-
	memoized_v(Q, M, VQM), !.
v(Q, M, VQM) :-
	Q = [0 | QT], !,
	v(QT, M, VQM).
v(Q, M, VQM) :-
	not(M = 0),
	not(memoized_v(Q, M, VQM)),
	Q = [QH | _QT],
	not(QH = 0),
	retractall(sum(Q, M, _)),
	assert(sum(Q, M, 0)),
	forall(
		(
			not(Q = []),
			valid_successor(Q, M, QQ),
			not(QQ = [])
		),
		(
			amount_consumption(QQ, C),
			MM is M - C,
			v(QQ, MM, VQQMM),
			sum(Q, M, Sum),
			NewSum is Sum + VQQMM,
			retract(sum(Q, M, Sum)),
			assert(sum(Q, M, NewSum))
		)
	),
	sum(Q, M, VQM),
	assert(memoized_v(Q, M, VQM)).

%===============================================================================
valid_successor(Q, RemainingAmount, QQ) :-
	resize(Q, RemainingAmount, Q0),
	InitialUpperRightSummand = 0,
	instanciate(Q0, InitialUpperRightSummand, RemainingAmount, QQ0),
	trim_right(QQ0, QQ),
	not(QQ = []).
%===============================================================================
repl(X, N, L) :-
    length(L, N),
    maplist(=(X), L).
resize(Q, N, QQ) :-
	M is max(0, N - 1),
	repl(0, M, L),
	append(Q, L, QQ).
trim_right(Q, QQ) :- reverse(Q, Q0), trim_left(Q0, QQ0), reverse(QQ0, QQ).
trim_left([], []).
trim_left(X, Y) :- X = [0 | XX], trim_left(XX, Y).
trim_left(X, Y) :- X = [H | _], not(H = 0), Y = X.
%===============================================================================
%   Q  *   UpperRightSummand
%     / \ /
%    *   *
%     \ /
%      *
%     /  QQ
%    *
% instanciate(+Q, +UpperRightSummand, +RemainingAmount, -QQ).
instanciate([], _UpperRightSummand, _RemainingAmount, []).
instanciate([Q | Qs], UpperRightSummand, RemainingAmount, [QQ | QQs]) :-
	Max is min(RemainingAmount, Q + UpperRightSummand),
	between(0, Max, QQ),
	NewUpperRightSummand = QQ,
	NewRemainingAmount is RemainingAmount - QQ,
	instanciate(Qs, NewUpperRightSummand, NewRemainingAmount, QQs).
%===============================================================================
amount_consumption([], 0).
amount_consumption([Q | Qs], C) :- amount_consumption(Qs, CC), C is CC + Q.
%===============================================================================
% ?- dict_create(D, toto, []), put_dict(key, D, value, DD).