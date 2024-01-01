%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Program: computation of the A279196 sequence
% Language: SWI-Prolog
% Author: Luc ROUSSEAU
% Date: 2023/12/27
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Principles:
% all possible Q's for a fixed N are built recursively.
% Progression is by antidiagonals.
% If an antidiagonal does not consume all the available amount of 
% 
%
%       / \   / \   / \   / \   / \   / \   / \   / \   / \   / \   / \ 
%      /   \ /   \ /   \ /   \ /   \ /   \ /   \ /   \ /   \ /   \ /   \
%     |     |     |     |     |     |Left |Right|     |     |     |     | => Q
%     |     |     |     |     |     |     |     |     |     |     |     |
%    / \   / \   / \   / \   / \   / \   / \   / \   / \   / \   / \   / \
%   /   \ /   \ /   \ /   \ /   \ /   \ /   \ /   \ /   \ /   \ /   \ /   \
%  |     |     |     |     |     |     |  X  |     |     |     |     |     | => QQ
%  |     |     |     |     |     |     |     |     |     |     |     |     |
%   \   / \   / \   / \   / \   / \   / \   / \   / \   / \   / \   / \   /
%    \ /   \ /   \ /   \ /   \ /   \ /   \ /   \ /   \ /   \ /   \ /   \ /
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% The memw predicate will memoize the w predicate,
% so memw must be declared dynamic.
:- dynamic(memw/2).

% clean/0 is det.
% Reinitializes the memoization. For interactive use.
clean :-
	retractall(memw(_, _)).

% main(+NMax) is det.
% Main entry point. Lists all the a(N)'s from N = 1 to N = NMax.
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

% a(+N, -AN) is det.
% AN provides the value of A27196(N).
a(N, AN) :- N = 1, !, AN = 1.
a(N, AN) :- M is N - 2, w([1] / M, AN).

% w(+Q / +M, -VQM) is det.
% The number of ways of filling something under Q
% - that respects the Pascal Triangle Inequalities constraints
% - and with a total sum of coefficients equal to M
% is equal to VQM.
w(_ / 0, 1) :- !.
w(Q / M, VQM) :- memw(Q / M, VQM), !.
w(Q / M, VQM) :-
	aggregate_all(sum(T), ww(Q / M, T), VQM),
	assert(memw(Q / M, VQM)),
	reverse(Q, QQ),
	((QQ = Q) -> true ; assert(memw(QQ / M, VQM))).

% ww(+Q / +M, -T) is nondet.
% a successor of Q / M has a w() equal to T
ww(Q / M, T) :-
	s_elision(0, Q, M, QQ, MM),
	w(QQ / MM, T).

% stop(+M, -QQ, -MM) is det.
% Manages the case when QQ stops immediately there: no further element.
stop(M, QQ, MM) :-
	QQ = [],
	MM = M.

% last(+Left, +M, -QQ, -MM).
% Manages the case when QQ stops immediately there
% The computed element is the last one and as such cannot be 0.
last(Left, M, QQ, MM) :-
	Max is min(Left, M),
	between(1, Max, X),
	QQ = [X],
	MM is M - X.

% ------------------------------------------------------------------------------
% The s_* predicates:
% - all have a (+Left, +Q, +M, -QQ, -MM) signature;
% - recursively build a successor of (Left, Q, M) and put the result in (QQ, M)
% where:
% - Left = the value in the previous row, on the "left of the cursor"
% - Q = the values in the previous row, on the "right" of the cursor"
% - M = the maximum sum of values that can be put in QQ
% - QQ = the current row being built from the previous one
% - MM = the remaining sum of values that one will have to put under QQ

% ------------------------------------------------------------------------------
% s_elision(+Left, +Q, +M, -QQ, -MM)
% manages the case when a sequence of leading zeroes is ongoing in QQ
% with the consequences that they are elidable and that one cannot stop there.

s_elision(Left, [], M, QQ, MM) :-
	last(Left, M, QQ, MM).

s_elision(Left, [Right | Rest], M, QQ, MM) :-
	Max is min(Left + Right, M),
	between(0, Max, X),
	M1 is M - X,
	(
		(X = 0)
	->
		(QQ = Y, s_elision(Right, Rest, M1, Y, MM))
	;
		(QQ = [X | Y], s_normal(Right, Rest, M1, Y, MM))
	).

% ------------------------------------------------------------------------------
% s_normal(+Left, +Q, +M, -QQ, -MM)
% manages the "normal" case when the previous number in QQ was not zero.

s_normal(Left, [], M, QQ, MM) :-
	stop(M, QQ, MM)
	;
	last(Left, M, QQ, MM).

s_normal(_Left, [Right | Rest], M, QQ, MM) :-
	stop(M, QQ, MM)
	;
	(
		X = 0,
		QQ = [X | Y],
		M1 is M - X,
		s_unbreakable(Right, Rest, M1, Y, MM)
	).

s_normal(Left, [Right | Rest], M, QQ, MM) :-
	Max is min(Left + Right, M),
	between(1, Max, X),
	QQ = [X | Y],
	M1 is M - X,
	s_normal(Right, Rest, M1, Y, MM).

% ------------------------------------------------------------------------------
% s_unbreakable(+Left, +Q, +M, -QQ, -MM)
% manages the case when a sequence of zeroes in ongoing in QQ
% but it's not a sequence of leading zeroes. These zeroes are internal.
% The consequence is: one cannot stop there.

s_unbreakable(Left, [], M, QQ, MM) :-
	last(Left, M, QQ, MM).

s_unbreakable(Left, [Right | Rest], M, [X | Y], MM) :-
	Max is min(Left + Right, M),
	between(0, Max, X),
	M1 is M - X,
	((X = 0) -> s_unbreakable(Right, Rest, M1, Y, MM) ; s_normal(Right, Rest, M1, Y, MM)).
