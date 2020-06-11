%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Title: a program for the computation of A335442
% Language: SWI-Prolog
% Author: Luc Rousseau
% Date: 10th June 2020
%
% Terms enumerated in lexicographic order of (n, s, k):
% for each n >= 1, for each s subset of 1..n with n-1 elements,
% for each k in 0..n-1,
% (sum_{t subset of s, Card(t)=k} prod_{x in t} x) is a term.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

main :-
	Nmax = 11,
	forall(between(1, Nmax, N),
	(
		range(N, Range),
		M is N - 1,
		forall(subsetk(Range, M, Subset),
		(
			% nl, % uncomment for legibility
			forall(between(0, M, K),
			(
				sum_prod(Subset, K, SumOfProducts),
				maplist(write, [SumOfProducts, ', '])
			)
			)
		)
		)
	)
	).

% ==============================================================================
% range(+N, -Range).
% det.
% Range is the set [1, ..., N]
range(N, Range) :-
	findall(Element, between(1, N, Element), Range).

% ==============================================================================
% sum_prod(+Set, +K, -SumOfProducts).
% det.
% SumOfProducts is the sum of the products of the subsets of Set that contain K elements.
sum_prod(Set, K, SumOfProducts) :-
	findall(Subset, subsetk(Set, K, Subset), Subsets),
	maplist(prod, Subsets, Products),
	sum(Products, SumOfProducts).

% ==============================================================================
% subsetk(+S, +N, -SS).
% multi.
% SS is a subset of S that contains N elements.
subsetk([], N, SS) :-
	!,
	N = 0,
	SS = [].
subsetk(S, N, _) :-
	length(S, L),
	L < N, !,
	fail.
subsetk(S, N, SS) :-
	length(S, L),
	L >= N,
	S = [T | Q],
	M is N - 1,
	(
		subsetk(Q, M, QQ),
		SS = [T | QQ]
	;
		subsetk(Q, N, SS)
	).

% ==============================================================================
% prod(+List, -Product).
% det.
% Product is the product of the elements in List.
prod(List, Product) :-
	prod(List, 1, Product).
prod(List, ProductSoFar, FinalProduct) :-
	List = [], !,
	FinalProduct = ProductSoFar.
prod(List, ProductSoFar, FinalProduct) :-
	List = [Head | Tail],
	NewProductSoFar is ProductSoFar * Head,
	prod(Tail, NewProductSoFar, FinalProduct).

% ==============================================================================
% sum(+List, -Sum).
% det.
% Sum is the sum of the elements in List.
sum(List, Sum) :-
	sum(List, 0, Sum).
sum(List, SumSoFar, FinalSum) :-
	List = [], !,
	FinalSum = SumSoFar.
sum(List, SumSoFar, FinalSum) :-
	List = [Head | Tail],
	NewSumSoFar is SumSoFar + Head,
	sum(Tail, NewSumSoFar, FinalSum).

% ==============================================================================
