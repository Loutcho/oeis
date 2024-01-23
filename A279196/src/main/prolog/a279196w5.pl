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
% - the choices for X depend on Left, Right, on the remaining mass M to distribute
%   and on the "mode" dictated by the history of choices that preceded X in LL.
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
%                                         |
%                                        / \     ----> direction of progression
%                                      current
%
%  Fundamental inequality: 0 <= X <= min(Left + Right, M)
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
% AN provides the value of A279196(N).
a(N, AN) :- N = 1, !, AN = 1.
a(N, AN) :- M is N - 2, w([1] / M, AN).

% w(+L / +M, -W) is det.
% The number of ways to Pascal-distribute the mass M under the list L is equal to W.
% Side effect: memoizes actual computations
w(_ / 0, 1) :- !.
w(L / M, W) :- memw(L / M, W), !.
w(L / M, W) :-
	aggregate_all(sum(WW), ww(L / M, WW), W),
	assert(memw(L / M, W)),
	reverse(L, L1),
	((L1 = L) -> true ; assert(memw(L1 / M, W))).

% ww(+L / +M, -WW) is nondet.
% a successor LL of L / M is such that w(LL, M - |LL|) is equal to WW.
ww(L / M, WW) :-
	successor(L / M, LL / MM),
	w(LL / MM, WW).

% successor(+L / +M, -LL) is nondet.
% LL is a successor of (L, M)
successor(L / M, LL / MM) :-
	build_successor(elision, 0, L, M, LL, MM).

% ------------------------------------------------------------------------------
% build_successor(+Mode, +Left, +L, +M, -LL, -MM) is nondet
%
% Builds a chunk of the successor thanks to chunks of the initial list.
% Intended for recursive usage: instantiates an element X and then calls
% itself with strictly smaller chunks located on the right of X.
% - Mode = a processing mode among:
%     - elision: zeroes on the left of L' are potentially to be elided;
%     - regular: the element of L' before X was not zero,
%       implying that it's always possible to stop with X;
%     - nobreak: a sequence of zeroes is ongoing,
%       implying it's not possible to stop with X.
% - Left = the value in the previous row, on the "left of the cursor"
% - L = the values in the previous row, on the "right" of the cursor"
% - M = the mass that can be Pascal distributed under Left and L
% - LL = the current row being built from the previous one
% - MM = the remaining mass that one will have to put under LL

build_successor(regular, _Left, _L, M, [], M).

build_successor(_Mode, Left, [], M, LL, MM) :-
	Max is min(Left, M),
	between(1, Max, X),
	LL = [X],
	MM is M - X.

build_successor(Mode, Left, [Right | Rest], M, LL, MM) :-
	Max is min(Left + Right, M),
	between(0, Max, X),
	M1 is M - X,
	update(Mode, X, LL, Y, NewMode),
	build_successor(NewMode, Right, Rest, M1, Y, MM).

update(Mode, X, LL, Y, NewMode) :-
	X = 0, !, % red cut
	append_rule_zero(Mode, Y, LL),
	mode_rule_zero(Mode, NewMode).

update(_Mode, X, [X | Y], Y, regular). % implicit: X > 0.

append_rule_zero(elision, Y,      Y ).
append_rule_zero(regular, Y, [0 | Y]).
append_rule_zero(nobreak, Y, [0 | Y]).

mode_rule_zero(elision, elision).
mode_rule_zero(regular, nobreak).
mode_rule_zero(nobreak, nobreak).

