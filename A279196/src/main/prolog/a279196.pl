a(N) :-
	M is N - 1,
	valid_chain_of_successors([1], M, Chain),
	writeln(Chain).
%===============================================================================
% valid_chain_of_successors(+Q, +RemainingAmount, -Chain)
% Chain is a valid list [Q1, Q2, ... Qn]
% such that Q1 is a valid successor of Q,
%           Q2 is a valid successor of Q1,
% ...       Qn is a valid successor of Qn-1,
% and RemainingAmount is totally consumed.


valid_chain_of_successors(_Q, 0, []).
valid_chain_of_successors(Q, RemainingAmount, Chain) :-
	not(RemainingAmount = 0),
	valid_successor(Q, RemainingAmount, QQ),
	not((Q = [], QQ = [])),
	amount_consumption(QQ, C),
	NewRemainingAmount is RemainingAmount - C,
	(
		NewRemainingAmount = 0
	->
		Chain = [QQ]
	;
		(
		valid_chain_of_successors(QQ, NewRemainingAmount, SubChain),
		Chain = [QQ | SubChain]
		)
	).

%===============================================================================
% valid_successor(Q, RemainingAmount, QQ).
% -QQ is a valid successor of +Q and has amount consumption at most +RemainingAmount.
valid_successor(Q, RemainingAmount, QQ) :-
	resize(Q, RemainingAmount, Q0),
	InitialUpperRightSummand = 0,
	instanciate(Q0, InitialUpperRightSummand, RemainingAmount, QQ0),
	trim_right(QQ0, QQ).
%===============================================================================
repl(X, N, L) :-
    length(L, N),
    maplist(=(X), L).

resize(Q, N, QQ) :-
	repl(0, N, L),
	append(Q, L, QQ).
%===============================================================================
% remove trailing zeros
trim_right(Q, QQ) :-
	reverse(Q, Q0),
	trim_left(Q0, QQ0),
	reverse(QQ0, QQ).
trim_left([], []).
trim_left(X, Y) :- X = [0 | XX], trim_left(XX, Y).
trim_left(X, Y) :- X = [H | _], not(H = 0), Y = X.
%===============================================================================
%
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
amount_consumption([Q | Qs], C) :-
	amount_consumption(Qs, CC),
	C is CC + Q.
%===============================================================================


% ?- dict_create(D, toto, []), put_dict(key, D, value, DD), put_dict(key2, DD, value2, DDD).
% D = toto{},
% DD = toto{key:value},
% DDD = toto{key:value, key2:value2}.