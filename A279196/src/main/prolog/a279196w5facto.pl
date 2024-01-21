%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Program: computation of the A279196 sequence
% Language: SWI-Prolog
% Author: Luc ROUSSEAU
% Date: 2023/12/27
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Principles:
% - all the numbers of possible Q's with a given mass are computed recursively.
% - the top level progression is by antidiagonals.
% - the progression in an antidiagonal is from left to right.
% - the choices for X depend on Left, Right, on the remaining mass to distribute
%   and on the "mode" dictated by the assignments that precede X in LL.
%
%       / \   / \   / \   / \   / \   / \   / \   / \   / \   / \   / \ 
%      /   \ /   \ /   \ /   \ /   \ /   \ /   \ /   \ /   \ /   \ /   \
%     |     |     |     |     |     |Left |Right|     |     |     |     | => L
%     |     |     |     |     |     |     |     |     |     |     |     |
%    / \   / \   / \   / \   / \   / \   / \   / \   / \   / \   / \   / \
%   /   \ /   \ /   \ /   \ /   \ /   \ /   \ /   \ /   \ /   \ /   \ /   \
%  |     |     |     |     |     |     |  X  |     |     |     |     |     | => LL
%  |     |     |     |     |     |     |     |     |     |     |     |     |
%   \   / \   / \   / \   / \   / \   / \   / \   / \   / \   / \   / \   /
%    \ /   \ /   \ /   \ /   \ /   \ /   \ /   \ /   \ /   \ /   \ /   \ /
%
%                           0 <= X <= min(M, Left + Right)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% The memw predicate will memoize the w predicate,
% so memw must be declared dynamic.
:- dynamic(memw/2).

% clean/0 is det.
% Reinitializes the memoization. For interactive use.
clean :-
	retractall(memw(_, _)).

% save/0 is det
% Saves the memoization to file. For interactive use.
save :-
	get_time(Now),
	format_time(atom(Filename), 'memw5_%Y%m%d_%H%M%S', Now),
	tell(Filename),
	forall(
		memw(L/M, W),
		maplist(write, [memw(L/M, W), '.\n'])
	),
	told.

% threaded_main/2 is det
% Runs main/1 in a separated thread with increased memory.
% Recommended entry point. E.g.: threaded_main(50, T) or threaded_main(50, _).
% The T that Prolog writes to the standard output is the one to use
% interactively in thread_join/2 to release the thread once the calculations are over.
threaded_main(NMax, Thread) :-
	thread_create(main(NMax), Thread, [stack_limit(10 000 000 000)]).

% main(+NMax) is det.
% Natural entry point but slightly awkward when run from the read-eval-print loop (interactive toplevel).
% Lists all the a(N)'s from N = 1 to N = NMax, with timestamps and durations.
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

% w(+L / +M, -W) is det.
% The number of ways to Pascal-distribute the mass M under the list L is equal to W.
% Side effect: memoizes actual computations
w(_ / 0, 1) :- !.
w(L / M, W) :- memw(L / M, W), !.
w(L / M, W) :-
	aggregate_all(sum(WW), ww(L / M, WW), W).
	%	assert(memw(L / M, W)),
	%	reverse(L, L1),
	%	((L1 = L) -> true ; assert(memw(L1 / M, W))).

% ww(+L / +M, -WW) is nondet.
% a successor LL of L / M is such that w(LL, M - |LL|) is equal to WW.
ww(L / M, WW) :-
	s_elision(0, L, M, LL, MM),
	w(LL / MM, WW).

% -----------------------------------------------------------------------------
% Technical predicates for code factorization:

% stop(+M, -LL, -MM) is det.
% Manages the common case when LL stops immediately: no current element.
stop(M, LL, MM) :-
	LL = [],
	MM = M.

% last(+Left, +M, -LL, -MM).
% Manages the common case when LL stops with the element being chosen which cannot be 0.
last(Left, M, LL, MM) :-
	Max is min(Left, M),
	between(1, Max, X),
	LL = [X],
	MM is M - X.

% ------------------------------------------------------------------------------
% The s_* predicates:
% - all have a (+Left, +L, +M, -LL, -MM) signature;
% - recursively build a successor of (Left, L, M) and put the result in (LL, MM)
% where:
% - Left = the value in the previous row, on the "left of the cursor"
% - L = the values in the previous row, on the "right" of the cursor"
% - M = the mass that can be Pascal distributed under Left and L
% - LL = the current row being built from the previous one
% - MM = the remaining sum of values that one will have to put under LL

% ------------------------------------------------------------------------------
% s_elision(+Left, +L, +M, -LL, -MM)
% manages the case when a sequence of leading zeroes is ongoing in LL
% with the consequences that they are elidable and that one cannot stop there.

s_elision(Left, [], M, LL, MM) :-
	last(Left, M, LL, MM).

s_elision(Left, [Right | Rest], M, LL, MM) :-
	Max is min(Left + Right, M),
	between(0, Max, X),
	M1 is M - X,
	(
		(X = 0)
	->
		(LL = Y, s_elision(Right, Rest, M1, Y, MM))
	;
		(LL = [X | Y], s_normal(Right, Rest, M1, Y, MM))
	).

% ------------------------------------------------------------------------------
% s_normal(+Left, +L, +M, -LL, -MM)
% manages the "normal" case when the previous number in LL was not zero.

s_normal(Left, [], M, LL, MM) :-
	stop(M, LL, MM)
	;
	last(Left, M, LL, MM).

s_normal(_Left, [Right | Rest], M, LL, MM) :-
	stop(M, LL, MM)
	;
	(
		X = 0,
		LL = [X | Y],
		M1 is M - X,
		s_unbreakable(Right, Rest, M1, Y, MM)
	).

s_normal(Left, [Right | Rest], M, LL, MM) :-
	Max is min(Left + Right, M),
	between(1, Max, X),
	LL = [X | Y],
	M1 is M - X,
	s_normal(Right, Rest, M1, Y, MM).

% ------------------------------------------------------------------------------
% s_unbreakable(+Left, +L, +M, -LL, -MM)
% manages the case when a sequence of zeroes in ongoing in LL
% but it's not a sequence of leading zeroes. These zeroes are internal.
% The consequence is: one cannot stop there.

s_unbreakable(Left, [], M, LL, MM) :-
	last(Left, M, LL, MM).

s_unbreakable(Left, [Right | Rest], M, [X | Y], MM) :-
	Max is min(Left + Right, M),
	between(0, Max, X),
	M1 is M - X,
	((X = 0) -> s_unbreakable(Right, Rest, M1, Y, MM) ; s_normal(Right, Rest, M1, Y, MM)).
