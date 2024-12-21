:- dynamic known_solution/1.

threaded_a(N, Thread) :-
	thread_create(a(N), Thread, [stack_limit(1 000 000 000)]).

a(N) :-
	NN is 2 * N,
	process(NN).

process(N) :-
	format(atom(S), '~|~`0t~d~2+', [N]),
	atom_concat('results.', S, Filename),
	tell(Filename),
	forall(
		(
			initial_state(N, InitialState),
			not_already_known_solution(InitialState, N, Solution)
		),
		(
			writeln(Solution),
			flush_output
		)
	),
	told.

initial_state(
	InitialNumberOfMoves,
	state(
		initial_number_of_moves(InitialNumberOfMoves),
		remaining_number_of_moves(InitialNumberOfMoves),
		already_visited_positions([point(0, 0)]),
		sequence(''),
		current_position(point(0, 0)),
		current_direction(dir(0, -1)),
		rotation_count(0)
	)
).

compute_new_direction(Direction, '.', Direction).
compute_new_direction(OldDirection, '<', NewDirection) :-
	OldDirection = dir(DX, DY),
	DXX is -DY,
	DYY is  DX,
	NewDirection = dir(DXX, DYY).
compute_new_direction(OldDirection, '>', NewDirection) :-
	OldDirection = dir(DX, DY),
	DXX is  DY,
	DYY is -DX,
	NewDirection = dir(DXX, DYY).

compute_new_position(point(X, Y), dir(DX, DY), point(XX, YY)) :-
	XX is X + DX,
	YY is Y + DY.

rotation_count_increment('<', +1).
rotation_count_increment('.',  0).
rotation_count_increment('>', -1).

constraint_when_x_equals_0_y_must_remain_nonnegative(X, Y) :-
	(
		X = 0
	->
		true
	;
		(
			Y >= 0
		->
			true
		;
			(
				% writeln('[x = 0 implies y >= 0]'),
				fail
			)
		)
	).

constraint_distance_position_start_is_bounded(X, Y, K) :-
	AX is abs(X),
	AY is abs(Y),
	(
		AX + AY =< K
	->
		true
	;
		(
			% writeln('[d <= k]'),
			fail
		)
	).

constraint_contour_winding_number_is_bounded(R, K) :-
	(
		abs(R - 4) =< K
	->
		true
	;
		(
			% writeln('[|r-4| <= k]'),
			fail
		)
	).

constraint_self_avoiding(K, NewPosition, AlreadyVisitedPositions) :-
	(
		K = 0
	->
		true
	;
		(
			not(member(NewPosition, AlreadyVisitedPositions))
		->
			true
		;
			(
				% writeln('[self avoiding]'),
				fail
			)
		)
	).

move(OldState, NewState) :- 

	OldState = state(
		initial_number_of_moves(M),
		remaining_number_of_moves(K),
		already_visited_positions(AlreadyVisitedPositions),
		sequence(Sequence),
		current_position(CurrentPosition),
		current_direction(CurrentDirection),
		rotation_count(RotationCount)
	),

	(
		CurrentPosition = point(0, 0)
	->
		Move = '<'
	;
		member(Move, ['<', '.', '>']) % < = turn left, + = move forward, > = turn right
	),
	atom_concat(Sequence, Move, NewSequence),

	compute_new_direction(CurrentDirection, Move, NewDirection),
	compute_new_position(CurrentPosition, NewDirection, NewPosition),
	NewPosition = point(XX, YY),

	constraint_when_x_equals_0_y_must_remain_nonnegative(XX, YY),

	KK is K - 1,

	constraint_distance_position_start_is_bounded(XX, YY, KK),

	rotation_count_increment(Move, RotationCountIncrement),
	NewRotationCount is RotationCount + RotationCountIncrement,

	constraint_contour_winding_number_is_bounded(NewRotationCount, KK),

	constraint_self_avoiding(KK, NewPosition, AlreadyVisitedPositions),
	
	NewState = state(
		initial_number_of_moves(M),
		remaining_number_of_moves(KK),
		already_visited_positions([NewPosition | AlreadyVisitedPositions]),
		sequence(NewSequence),
		current_position(NewPosition),
		current_direction(NewDirection),
		rotation_count(NewRotationCount)
	).

desmos(point(P, Q) - point(PP, QQ), A) :-
	format(atom(A), '((1-t)*(~d)+t*(~d),(1-t)*(~d)+t*(~d))', [P, PP, Q, QQ]).

not_already_known_solution(State0, Depth, Solution) :-
	iterate(move, Depth, State0, State1),
	State1 =.. Fields,
	member(sequence(Sequence), Fields),
	class_representative(Sequence, ClassRepresentative),
	Solution = ClassRepresentative,
	Goal = known_solution(Solution),
	(
		Goal
	->
		fail
	;
		assert(Goal)
	).

iterate(_PredicateName, N, In, Out) :-
	N = 0, !,
	Out = In.
iterate(PredicateName, N, In, Out) :-
	Goal =.. [PredicateName, In, Tmp],
	Goal,
	M is N - 1,
	iterate(PredicateName, M, Tmp, Out).
	
equivalent(Sequence, Equivalent) :-
	%(Sequence1 = Sequence ; symmetric(Sequence, Sequence1)),
	Sequence1 = Sequence,
	(Sequence2 = Sequence1 ; term_reverse(Sequence1, Sequence2)),
	circular_permutation(Sequence2, Equivalent).

term_reverse(X, Y) :-
	atom_chars(X, XX),
	reverse(XX, YY),
	atom_chars(Y, YY).

s('<', '>').
s('>', '<').
s('+', '+').

symmetric(SequenceIn, SequenceOut) :-
	atom_chars(SequenceIn, CharsIn),
	maplist(s, CharsIn, CharsOut),
	atom_chars(SequenceOut, CharsOut).

circular_permutation(Sequence, Equivalent) :-
	atom_concat(Begin, End, Sequence),
	atom_length(End, L),
	L > 0,
	atom_concat(End, Begin, Equivalent).

class_representative(Sequence, ClassRepresentative) :-
	findall(Equivalent, equivalent(Sequence, Equivalent), Equivalents),
	sort(Equivalents, SortedEquivalents),
	nth1(1, SortedEquivalents, ClassRepresentative).

