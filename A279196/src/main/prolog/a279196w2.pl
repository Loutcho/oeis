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
	valid_successor(Q / M, QQ / MM),
	w(QQ / MM, T).

valid_successor(Q / M, QQ / MM) :-
	valid_successor(0, elision(true), Q, M, QQ, MM),
	not(MM = M).

%                / \
%               |   |
%              / \ / \
%             |   |   |
%            / \ / \ / \
%           |   |   |   |
%          / \ / \ / \ / \
%         |   |   |   |   |
%        / \ / \ / \ / \ / \
%       |   |   |   |   |   |
%        \ / \ / \ / \ / \ /

valid_successor(Left, elision(Elision), [], M, QQ, MM) :-
	Max is min(M, Left),
	between(0, Max, X),
	((X + Elision = 0 + true) -> (QQ = []) ; (QQ = [X])),
	MM is M - X.

valid_successor(Left, elision(ElisionIn), [Right | Rest], M, QQ, MMM) :-
	Max is min(M, Left + Right),
	between(0, Max, X),
	((X = 0, ElisionIn = true) -> (QQ = Y, ElisionOut = true) ; true),
	((X = 0, ElisionIn = false) -> (QQ = [0 | Y], ElisionOut = false) ; true),
	(not(X = 0) -> (QQ = [X | Y], ElisionOut = false) ; true),
	MM is M - X,
	valid_successor(Right, elision(ElisionOut), Rest, MM, Y, MMM).

trim_left([], []).
trim_left(X, Y) :- X = [0 | XX], trim_left(XX, Y).
trim_left(X, Y) :- X = [H | _], not(H = 0), Y = X.
