%-------------------------------------------------------------------------------
% threaded_a(+N, -Thread) is det.
% Thread is the thread in which a(N) is executed.
% -> Executes a(N) in a separate thread, to avoid freeze of GUI.

threaded_a(N, Thread) :-
	thread_create(time(a(N)), Thread, [stack_limit(1 000 000 000)]).

%-------------------------------------------------------------------------------
% a(+N) is det.
% No logical meaning. Equivalent to true.
% Computes a(N) by side effect (logs).

a(N) :-
	log(debug, [begin]),
	NN is 2 * N,
	process(NN),
	log(debug, [end]).

%-------------------------------------------------------------------------------
% process(+N) is det.
% No logical meaning. Equivalent to true.
% Computes the number of closed, self-avoiding, lattice paths of length N.
% By side effects (memoization + logs).
process(N) :-
	format(atom(S), '~|~`0t~d~2+', [N]),
	atom_concat('nresults.', S, Filename),
	tell(Filename),
	forall(
		(
			initial_state(N, InitialState),
			solution(InitialState, N, Solution)
		),
		(
			log(info, [Solution])
		)
		%(writeln(Solution), flush_output)
	),
	told.

%-------------------------------------------------------------------------------
% initial_state(+InitialNumberOfMoves, -State) is det.
% State is the initialised structure that suits the needs of the computation
% for perimeter InitialNumberOfMoves.

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

%-------------------------------------------------------------------------------
% solution(+StateIn, +Depth, -Solution) is nondet.

solution(State0, Depth, Solution) :-
	iterate(move, Depth, State0, State1),
	State1 =.. Fields,
	member(sequence(Sequence), Fields),
	class_representative(Sequence, ClassRepresentative),
	snake_string(ClassRepresentative, SnakeString),
	atomic_list_concat([Sequence, ClassRepresentative, SnakeString], '|', Solution).

compute_new_direction(Direction, '=', Direction).
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
rotation_count_increment('=',  0).
rotation_count_increment('>', -1).

constraint_x_must_remain_nonnegative(X) :-
	(
		X >= 0
	->
		true
	;
		(
				log(debug, ['(fail) x should be >= 0']),
				fail
		)
	).

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
				log(debug, ['(fail) when x = 0, y should be >= 0']),
				fail
			)
		)
	).

% The Manhattan-distance from current position to the origin should stay
% less than or equal to the number of remaining moves
constraint_distance_position_start_is_bounded(X, Y, K) :-
	AX is abs(X),
	AY is abs(Y),
	(
		AX + AY =< K
	->
		true
	;
		(
			log(debug, ['(fail) d should be <= k']),
			fail
		)
	).

% The overall number of quarter turns being +4
% (we prune the case where it is -4, which is symmetrical),
% the distance between the current number of quarter turns and 4
% must be less than or equal to the number of remaining moves.
constraint_contour_winding_number_is_bounded(R, K) :-
	(
		K - abs(R - 4) >= 0
	->
		true
	;
		(
			log(debug, ['(fail) k - |r-4| should be >= 0']),
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
				log(debug, ['(fail) must be self-avoiding']),
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
		member(Move, ['<', '=', '>']) % < turn left, = move forward, > turn right
	),
	atom_concat(Sequence, Move, NewSequence),
	log(debug, ['Exploring: ', NewSequence]),

	compute_new_direction(CurrentDirection, Move, NewDirection),
	compute_new_position(CurrentPosition, NewDirection, NewPosition),
	NewPosition = point(XX, YY),

	constraint_x_must_remain_nonnegative(XX),
	% constraint_when_x_equals_0_y_must_remain_nonnegative(XX, YY),

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
	
% Makes a snake string, usable on the drawing website
% https://www.antiton.de/snake/index.html
% And especially with paramters in URL, it is the part after the "+", e.g.:
% https://www.antiton.de/snake/index.html?s=12+0000CC0000C 
snake_string(S, SS) :-
	atom_chars(S, C),
	maplist(t, C, CC),
	atom_chars(SS, CC).

t('<', 'c').
t('=', '0').
t('>', 'C').

desmos(point(P, Q) - point(PP, QQ), A) :-
	format(atom(A), '((1-t)*(~d)+t*(~d),(1-t)*(~d)+t*(~d))', [P, PP, Q, QQ]).



iterate(_PredicateName, N, In, Out) :-
	N = 0, !,
	Out = In.
iterate(PredicateName, N, In, Out) :-
	Goal =.. [PredicateName, In, Tmp],
	Goal,
	M is N - 1,
	iterate(PredicateName, M, Tmp, Out).

%-------------------------------------------------------------------------------
% class_representative(+Sequence, -ClassRepresentative) is det.
% ClassRepresentative is the class representative of this sequence, i.e.,
% among all the equivalents to Sequence, it is the smallest one in
% lexicographic order of the ASCII codes of the letters '<', '=', '>'.

class_representative(Sequence, ClassRepresentative) :-
	findall(Equivalent, equivalent(Sequence, Equivalent), Equivalents),
	sort(Equivalents, SortedEquivalents),
	nth1(1, SortedEquivalents, ClassRepresentative).
	
%-------------------------------------------------------------------------------
% equivalent(+Sequence, -Equivalent) is multi
% Equivalent is an equivalent sequence to the provided sequence Sequence.
% "Equivalent to" = "is a circ. perm. of or is a circ. perm. of the reverse of"
% -> generates all the equivalents of Sequence with backtracking.
equivalent(Sequence, Equivalent) :-
	term_reverse(Sequence, Reverse),
	(Reverse = Sequence -> X = [Sequence] ; X = [Sequence, Reverse]),
	member(E, X),
	circular_permutation(E, Equivalent).

%-------------------------------------------------------------------------------
% term_reverse(+X, -Y) is det
% Y is the reverse term of X.
term_reverse(X, Y) :-
	atom_chars(X, XX),
	reverse(XX, YY),
	atom_chars(Y, YY).

%-------------------------------------------------------------------------------
% circular_permutation(+Sequence, -CircularPermutation) is multi
% CircularPermutation is a circular permutation of Sequence
% -> generates all the circular permutations of Sequence with backtracking.
circular_permutation(Sequence, CircularPermutation) :-
	atom_concat(Begin, End, Sequence),
	atom_length(End, L),
	L > 0,
	atom_concat(End, Begin, CircularPermutation).

%-------------------------------------------------------------------------------

log_conf(debug, discard).
log_conf(info, keep).

log(Level, Message) :-
	log_conf(Level, Conf),
	(
		Conf = discard
	;
		(
			Conf = keep,
			real_log(Level, Message)
		)
	).

real_log(Level, Message) :-
	get_time(Now),
	format_time(atom(Timestamp), '%Y/%m/%d %H:%M:%S ', Now),
	format(atom(FLevel), '~|~` t~a~5+|', Level),
	maplist(write, [Timestamp, FLevel | Message]),
	nl,
	flush_output.
