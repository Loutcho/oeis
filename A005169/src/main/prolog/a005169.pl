%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% A005169
% Enumerates / counts the number of fountains with n coins
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

main :-
	forall(
		between(0, 25, N),
		(
			a(N, AN),
			maplist(write, [AN, ', ']),
			flush_output
		)
	).

% a(+N, -AN) is det.
% AN is the number of fountains with N coins.
a(N, AN) :-
	aggregate_all(count, build_fountain(N, _), AN).

% draw_all_fountains(+N) is det.
% Writes to the output all the fountains with N coins.
draw_all_fountains(N) :-
	forall(
		build_fountain(N, Fountain),
		draw_fountain(Fountain)
	).
draw_fountain(Fountain) :-
	draw_layer(0, Fountain),
	writeln('-------------------').
draw_layer(_, []).
draw_layer(D, [Layer | Rest]) :-
	DD is D + 1,
	draw_layer(DD, Rest),
	format(atom(Indent), '~t~*|', [D]),
	write(Indent),
	maplist(draw_pos, Layer),
	nl.
draw_pos(0) :- write('  ').
draw_pos(1) :- write('()').

% build_fountain(+NbCoins, -Fountain) is multi.
% Fountain is a fountain with NbCoins coins.
build_fountain(NbCoins, [Base | OverBase]) :-
	build_base(NbCoins, Base, LengthBase),
	RemainingCoins is NbCoins - LengthBase,
	build_over_base(Base, RemainingCoins, OverBase).

% build_base(+NbCoins, -Base, -LengthBase) is multi.
% Base is a possible base (= first row) of coins
% when building a fountain with NbCoins coins,
% and the length of Base is LengthBase.
build_base(N, Base, LengthBase) :-
	TN is ceiling((sqrt(1.0 + 8.0 * N) - 1) / 2.0), % triangular root of N
	between(TN, N, LengthBase),
	length(Base, LengthBase),
	maplist(=(1), Base).

% build_over_base(+Base, +RemainingCoins, -OverBase) is multi.
% OverBase is a way of completing the fountain above the base Base,
% with the remaining RemainingCoins coins.
build_over_base(_, 0, []).
build_over_base(Base, RemainingCoins, OverBase) :-
	RemainingCoins > 0,
	build_over_layer(Base, RemainingCoins, OverBase).

% build_over_layer(+Layer, +RemainingCoins, -OverLayer) is multi.
% OverLayer is a way of completing the fountain above the layer Layer,
% with the remaining RemainingCoins coins.
build_over_layer(Layer, 0, []) :-
	Layer = [_], !.
build_over_layer(Layer, RemainingCoins, OverLayer) :-
	pairs(Layer, Pairs),
	build_layer(Pairs, RemainingCoins, NewLayer, NewRemainingCoins),
	(
		NewRemainingCoins = RemainingCoins % if no coin was used
	-> % then the new layer is an empty one, and the building can be stopped
		(
				RemainingCoins = 0 % if no coin remains to be distributed
			-> % then that's a success and we cut the building just above Layer
				OverLayer = [] 
			; % otherwise that's a failure
				fail
		)
	; % otherwise the new layer is not empty, and the rest is obtained recursively
		(
			OverLayer = [NewLayer | NewOverLayer],
			build_over_layer(NewLayer, NewRemainingCoins, NewOverLayer)
		)
	).

% pairs(+List, -Pairs) is det.
% Pairs is the list of ordered pairs of consecutive elements in List.
pairs([_], []).	
pairs([X, Y | R], [X - Y | P]) :-
	pairs([Y | R], P).

% build_layer(+Pairs, +RemainingCoins, -Layer, -NewRemainingCoins) is multi.
% Layer is a way of filling a layer with at most RemainingCoins coins above Pairs.
% The new number of remaining coins after that filling is NewRemainingCoins.
build_layer([], R, [], R).
build_layer([Pair | OtherPairs], RemainingCoins, [Coin | OtherCoins], NewRemainingCoins) :-
	make_a_choice(Pair, RemainingCoins, Coin),
	TmpRemainingCoins is RemainingCoins - Coin,
	build_layer(OtherPairs, TmpRemainingCoins, OtherCoins, NewRemainingCoins).

% make_a_choice((+Left) - (+Right), +RemainingCoins, -Choice) is multi.
% Left, Right and Choice are presence flags (0 = no coin, 1 = coin) of a coin.
% When presence flag is Left in below-left position compared to current position,
% and presence flag is Right in below-right position compared to current position,
% and the number of remainingCoins to be distributed is RemainingCoins,
% then Choice is a choosable presence flag for the current position.
make_a_choice(_, _, 0).
make_a_choice(1 - 1, R, 1) :-
	R > 0.
