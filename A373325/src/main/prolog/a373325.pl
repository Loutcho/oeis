%
% Title: a Prolog program for A373325
% Author: Luc Rousseau
% Date: 2024/06/05
% Language: SWI-Prolog
%

%===============================================================================
% MAIN
%===============================================================================

main :-
	forall(
		between(0, 10, N),
		(
			a(N, AN),
			maplist(write, [AN, ', ']),
			flush_output
		)
	).

a(N, AN) :-
	initial_state(S0),
	aggregate_all(
		count,
		chain_moves(S0, N, _S1),
		AN
	).

%===============================================================================
% STATE
%===============================================================================

initial_state(S) :-
	unique_id(H1), unique_id(H2), unique_id(EE),
	unique_id(H1_EE), unique_id(EE_H2), unique_id(H2_H1),
	S = state(
		nodes([
			node(id(H1), number(0), prev(H2_H1), next(H1_EE)),
			node(id(EE), number(1), prev(H1_EE), next(EE_H2)),
			node(id(H2), number(0), prev(EE_H2), next(H2_H1))
		]),
		edges([
			edge(id(H1_EE), prev(H1), next(EE), partner(EE_H2)),
			edge(id(EE_H2), prev(EE), next(H2), partner(H1_EE)),
			edge(id(H2_H1), prev(H2), next(H1), partner(none))
		]),
		seed(EE)
	).

state_get_node(State, Id, Node) :-
	State = state(nodes(Nodes), _Edges, _Seed),
	Node = node(id(Id), _Number, _Prev, _Next),
	member(Node, Nodes).

state_add_node(StateIn, Node, StateOut) :-
	StateIn = state(nodes(Nodes), Edges, Seed),
	StateOut = state(nodes([Node | Nodes]), Edges, Seed).

state_upd_node(StateIn, NewNode, StateOut) :-
	StateIn = state(nodes(Nodes), Edges, Seed),
	NewNode = node(id(Id), _Number, _Prev, _Next),
	OldNode = node(id(Id), _, _, _),
	select(OldNode, Nodes,  NodesWithoutOldNode),
	NewNodes = [NewNode | NodesWithoutOldNode],
	StateOut = state(nodes(NewNodes), Edges, Seed).

state_del_node(StateIn, Node, StateOut) :-
	StateIn = state(nodes(Nodes), Edges, Seed),
	select(node(id(Node), number(_), prev(_), next(_)), Nodes, NewNodes),
	StateOut = state(nodes(NewNodes), Edges, Seed).

state_add_nodes(State, [], State).
state_add_nodes(StateIn, [Head | Tail], StateOut) :-
	state_add_node(StateIn, Head, StateTmp),
	state_add_nodes(StateTmp, Tail, StateOut).

state_upd_nodes(State, [], State).
state_upd_nodes(StateIn, [Head | Tail], StateOut) :-
	state_upd_node(StateIn, Head, StateTmp),
	state_upd_nodes(StateTmp, Tail, StateOut).

state_del_nodes(State, [], State).
state_del_nodes(StateIn, [Head | Tail], StateOut) :-
	state_del_node(StateIn, Head, StateTmp),
	state_del_nodes(StateTmp, Tail, StateOut).

state_get_edge(State, Id, Edge) :-
	State = state(_Nodes, edges(Edges), _Seed),
	Edge = edge(id(Id), _Prev, _Next, _Partner),
	member(Edge, Edges).

state_add_edge(StateIn, Edge, StateOut) :-
	StateIn = state(Nodes, edges(Edges), Seed),
	StateOut = state(Nodes, edges([Edge | Edges]), Seed).

state_upd_edge(StateIn, NewEdge, StateOut) :-
	StateIn = state(Nodes, edges(Edges), Seed),
	NewEdge = edge(id(Id), _Prev, _Next, _Partner),
	OldEdge = edge(id(Id), _, _, _),
	(
		select(OldEdge, Edges, EdgesWithoutOldEdge)
		-> true
		;
		(
			writeln('Unexpected error'),
			writeln('Id' = Id),
			maplist(edge_get_id, Edges, Ids),
			writeln('Ids' = Ids),
			fail
		)
	),
	NewEdges = [NewEdge | EdgesWithoutOldEdge],
	StateOut = state(Nodes, edges(NewEdges), Seed).

state_del_edge(StateIn, Edge, StateOut) :-
	StateIn = state(Nodes, edges(Edges), Seed),
	select(edge(id(Edge), prev(_), next(_), partner(_)), Edges, NewEdges),
	StateOut = state(Nodes, edges(NewEdges), Seed).

state_add_edges(State, [], State).
state_add_edges(StateIn, [Head | Tail], StateOut) :-
	state_add_edge(StateIn, Head, StateTmp),
	state_add_edges(StateTmp, Tail, StateOut).

state_upd_edges(State, [], State).
state_upd_edges(StateIn, [Head | Tail], StateOut) :-
	state_upd_edge(StateIn, Head, StateTmp),
	state_upd_edges(StateTmp, Tail, StateOut).

state_del_edges(State, [], State).
state_del_edges(StateIn, [Head | Tail], StateOut) :-
	state_del_edge(StateIn, Head, StateTmp),
	state_del_edges(StateTmp, Tail, StateOut).

state_upd_seed(StateIn, NewSeed, StateOut) :-
	StateIn = state(Nodes, Edges, _OldSeed),
	StateOut = state(Nodes, Edges, seed(NewSeed)).

state_get_seed(State, Seed) :-
	State = state(_Nodes, _Edges, seed(Seed)).

%===============================================================================
% NODES
%===============================================================================

construct_node(Id, Number, Prev, Next, Node) :-
	Node = node(id(Id), number(Number), prev(Prev), next(Next)).

node_get_number(Node, Number) :-
	Node = node(_Id, number(Number), _Prev, _Next).

node_get_prev(Node, Prev) :-
	Node = node(_Id, _Number, prev(Prev), _Next).

node_get_next(Node, Next) :-
	Node = node(_Id, _Number, _Prev, next(Next)).

node_upd_prev(NodeIn, NewPrev, NodeOut) :-
	NodeIn = node(Id, Number, prev(_), Next),
	NodeOut = node(Id, Number, prev(NewPrev), Next).
	
node_upd_next(NodeIn, NewNext, NodeOut) :-
	NodeIn = node(Id, Number, Prev, next(_)),
	NodeOut = node(Id, Number, Prev, next(NewNext)).

%===============================================================================
% EDGES
%===============================================================================

construct_edge(Id, Prev, Next, Partner, Edge) :-
	Edge = edge(id(Id), prev(Prev), next(Next), partner(Partner)).

edge_get_id(Edge, Id) :-
	Edge = edge(id(Id), _Prev, _Next, _Partner).

edge_get_prev(Edge, Prev) :-
	Edge = edge(_Id, prev(Prev), _Next, _Partner).

edge_get_next(Edge, Next) :-
	Edge = edge(_Id, _Prev, next(Next), _Partner).
	
edge_get_partner(Edge, Partner) :-
	Edge = edge(_Id, _Prev, _Next, partner(Partner)).

edge_is_traversable(EdgeObject) :-
	edge_get_partner(EdgeObject, Partner),
	not(Partner = none).

edge_upd_prev(EdgeIn, NewPrev, EdgeOut) :-
	EdgeIn = edge(Id, prev(_), Next, Partner),
	EdgeOut = edge(Id, prev(NewPrev), Next, Partner).

edge_upd_next(EdgeIn, NewNext, EdgeOut) :-
	EdgeIn = edge(Id, Prev, next(_), Partner),
	EdgeOut = edge(Id, Prev, next(NewNext), Partner).

%===============================================================================
% MOVES
%===============================================================================

chain_moves(S, Depth, SS) :- Depth = 0, !, SS = S.
chain_moves(S, Depth, SS) :-
	move(S, S0),
	NewDepth is Depth - 1,
	chain_moves(S0, NewDepth, SS).

move(S, SS) :-
	legal_move(S, M),
	make_move(S, M, SS).

next(State, EdgeId, NextEdgeId) :-
	state_get_edge(State, EdgeId, EdgeObject),
	edge_get_next(EdgeObject, NodeId),
	state_get_node(State, NodeId, NodeObject),
	node_get_next(NodeObject, NextEdgeId).

orbit(State, Edge, [Edge | OtherEdges]) :-
	next(State, Edge, NextEdge),
	orbit2(State, Edge, NextEdge, OtherEdges).

orbit2(_, StartEdge, StartEdge, []).
orbit2(State, StartEdge, Edge, [Edge | OtherEdges]) :-
	not(Edge = StartEdge),
	next(State, Edge, NextEdge),
	orbit2(State, StartEdge, NextEdge, OtherEdges).

legal_move(State, Move) :-
	state_get_seed(State, Seed),
	state_get_node(State, Seed, NodeSeed),
	node_get_next(NodeSeed, FirstEdge),
	orbit(State, FirstEdge, SurroundingEdgeIds),
	maplist(state_get_edge(State), SurroundingEdgeIds, SurroundingEdgeObjects),
	include(edge_is_traversable, SurroundingEdgeObjects, TraversableSurroundingEdgeObjects),
	member(Edge, TraversableSurroundingEdgeObjects),
	Move = move(Edge).

make_move(S, M, SS) :-
	S = state(_Nodes, _Edges, seed(Seed)),
	M = move(Edge),
	Edge = edge(_Id, prev(Prev), next(Next), _Partner),
	(
		(Next = Seed, cross_left(S, SS))
	;
		(Prev = Seed, cross_right(S, SS))
	;
		(not(Next = Seed), not(Prev = Seed),
		cross(S, Edge, SS))
	).

%===============================================================================
% CROSS LEFT
%===============================================================================

cross_left(StateIn, StateOut) :-

	state_get_seed(StateIn, Seed),
	S1 = Seed, unique_id(S2), unique_id(S3), unique_id(S4), unique_id(TT),
	unique_id(S1_S2), unique_id(S2_TT), unique_id(TT_S3), unique_id(S4_S4),
	state_get_node(StateIn, Seed, node(id(Seed), number(M), prev(Q), next(R))),
	N is M + 1,

	construct_node(S1, M,     Q, S1_S2, Node_S1),
	construct_node(S2, M, S1_S2, S2_TT, Node_S2),
	construct_node(TT, N, S2_TT, TT_S3, Node_TT),
	construct_node(S3, M, TT_S3,     R, Node_S3),
	construct_node(S4, M, S4_S4, S4_S4, Node_S4),
	construct_edge(S1_S2, S1, S2, S4_S4, Edge_S1_S2),
	construct_edge(S2_TT, S2, TT, TT_S3, Edge_S2_TT),
	construct_edge(TT_S3, TT, S3, S2_TT, Edge_TT_S3),
	construct_edge(S4_S4, S4, S4, S1_S2, Edge_S4_S4),
	
	state_upd_node(StateIn , Node_S1, State001),
	state_add_nodes(State001, [Node_S2, Node_TT, Node_S3, Node_S4], State002),
	state_add_edges(State002, [Edge_S1_S2, Edge_S2_TT, Edge_TT_S3, Edge_S4_S4], State003),
	state_get_edge(State003, R, Edge_R),
	edge_upd_prev(Edge_R, S3, NewEdge_R),
	state_upd_edge(State003, NewEdge_R, State004),
	state_upd_seed(State004, TT, StateOut).

%===============================================================================
% CROSS RIGHT
%===============================================================================

cross_right(StateIn, StateOut) :-

	state_get_seed(StateIn, Seed),
	S1 = Seed, unique_id(S2), unique_id(S3), unique_id(S4), unique_id(TT),
	unique_id(S1_TT), unique_id(TT_S2), unique_id(S2_S3), unique_id(S4_S4),
	state_get_node(StateIn, Seed, node(id(Seed), number(M), prev(Q), next(R))),
	N is M + 1,
	
	construct_node(S1, M,     Q, S1_TT, Node_S1),
	construct_node(TT, N, S1_TT, TT_S2, Node_TT),
	construct_node(S2, M, TT_S2, S2_S3, Node_S2),
	construct_node(S3, M, S2_S3,     R, Node_S3),
	construct_node(S4, M, S4_S4, S4_S4, Node_S4),
	construct_edge(S1_TT, S1, TT, TT_S2, Edge_S1_TT),
	construct_edge(TT_S2, TT, S2, S1_TT, Edge_TT_S2),
	construct_edge(S2_S3, S2, S3, S4_S4, Edge_S2_S3),
	construct_edge(S4_S4, S4, S4, S2_S3, Edge_S4_S4),
	
	state_upd_node(StateIn, Node_S1, State001),
	state_add_nodes(State001, [Node_TT, Node_S2, Node_S3, Node_S4], State002),
	state_add_edges(State002, [Edge_S1_TT, Edge_TT_S2, Edge_S2_S3, Edge_S4_S4], State003),
	state_get_edge(State003, R, Edge_R),
	edge_upd_prev(Edge_R, S3, NewEdge_R),
	state_upd_edge(State003, NewEdge_R, State004),
	state_upd_seed(State004, TT, StateOut).

%===============================================================================
% CROSS
%===============================================================================

cross(StateIn, Edge, StateOut) :-

	state_get_seed(StateIn, E),
	state_get_node(StateIn, E, NodeSeed),
	node_get_number(NodeSeed, M),
	node_get_prev(NodeSeed, C),
	node_get_next(NodeSeed, D),
	
	Edge_W1 = Edge,
	edge_get_id(Edge_W1, W1),
	edge_get_prev(Edge_W1, A1),
	edge_get_next(Edge_W1, B1),
	edge_get_partner(Edge_W1, W2),

	state_get_edge(StateIn, W2, Edge_W2),
	Edge_W2 = edge(id(W2), prev(B2), next(A2), partner(W1)),
	edge_get_prev(Edge_W2, B2),
	edge_get_next(Edge_W2, A2),
	
	state_get_node(StateIn, A1, Node_A1),
	state_get_node(StateIn, A2, Node_A2),
	state_get_node(StateIn, B1, Node_B1),
	
	state_get_edge(StateIn, C, Edge_C),
	state_get_edge(StateIn, D, Edge_D),
	
	N is M + 1,
	unique_id(E1), unique_id(E2), unique_id(E3), unique_id(E4), unique_id(F),
	unique_id(X), unique_id(Y), unique_id(XX), unique_id(YY), unique_id(U), unique_id(V),

	construct_node(E1, M, C , Y , Node_E1),
	construct_node(E2, M, X , D , Node_E2),
	construct_node(E3, M, YY, U , Node_E3),
	construct_node(E4, M, V , XX, Node_E4),
	construct_node(F , N, U , V , Node_F ),

	construct_edge(U , E3, F , V , Edge_U ),
	construct_edge(V , F , E4, U , Edge_V ),
	construct_edge(X , A1, E2, XX, Edge_X ),
	construct_edge(Y , E1, B1, YY, Edge_Y ),
	construct_edge(XX, E4, A2, X , Edge_XX),
	construct_edge(YY, B2, E3, Y , Edge_YY),

	edge_upd_next(Edge_C , E1, NewEdge_C ), state_upd_edge(StateIn, NewEdge_C, State001),
	edge_upd_prev(Edge_D , E2, NewEdge_D ), state_upd_edge(State001, NewEdge_D, State002),
	node_upd_next(Node_A1, X , NewNode_A1), state_upd_node(State002, NewNode_A1, State003),
	node_upd_prev(Node_B1, Y , NewNode_B1), state_upd_node(State003, NewNode_B1, State004),
	node_upd_prev(Node_A2, XX, NewNode_A2), state_upd_node(State004, NewNode_A2, State005),
	% B2 might be the same node as A2, reason why we delayed its reading
	state_get_node(State005, B2, Node_B2),
	node_upd_next(Node_B2, YY, NewNode_B2), state_upd_node(State005, NewNode_B2, State006),

	state_del_node(State006,  E, State007),
	state_del_edge(State007, W1, State008),
	state_del_edge(State008, W2, State009),

	state_add_nodes(State009, [Node_E1, Node_E2, Node_E3, Node_E4, Node_F], State010),
	state_add_edges(State010, [Edge_U, Edge_V, Edge_X, Edge_Y, Edge_XX, Edge_YY], State011),
	
	state_upd_seed(State011, F, StateOut).

%===============================================================================
% GRAPHVIZ ROUTINES
%===============================================================================
% Useful for debugging.
% Best rendered with neato.

graphviz_node(Node) :-
	Node = node(id(Id), number(Number), prev(Prev), next(Next)),
	maplist(write, ['\t"', Id, '" [shape = "box" style = "filled" fillcolor = "beige" label = "', Id, '\\n', Number, '"];\n']),
	maplist(write, ['\t"', Id, '" -> "', Prev, '" [penwidth = "0.2" color = "green"];\n']),
	maplist(write, ['\t"', Id, '" -> "', Next, '" [penwidth = "4" color = "green"];\n']).

graphviz_edge(Edge) :-
	Edge = edge(id(Id), prev(Prev), next(Next), partner(Partner)),
	maplist(write, ['\t"', Id, '" [shape = "point"];\n']),
	maplist(write, ['\t"', Id, '" -> "', Prev, '" [penwidth = "0.2" color = "green"];\n']),
	maplist(write, ['\t"', Id, '" -> "', Next, '" [penwidth = "4" color = "green"];\n']),
	(
		Partner = none
		-> maplist(write, ['\t"none" [shape = "none" label = " &#x20E0;"];\n'])
		; true
	),
	maplist(write, ['\t"', Id, '" -> "', Partner, '" [style = "dashed" penwidth="0.2" color = "blue"];\n']).

graphviz_state(State) :-
	unique_id(X),
	atom_concat('C:\\tmp\\', X, OutputFile), % adapt the path to your needs
	tell(OutputFile),
	State = state(
		nodes(Nodes),
		edges(Edges),
		seed(Seed)
	),
	maplist(write, ['digraph G {\n']),
	maplist(write, ['\tseed [shape = "none" label = "&#9765;" fontsize = "28.0"];\n']),
	maplist(write, ['\tseed -> "', Seed, '" [dir = "both" arrowhead = "none" arrowtail = "onormal" style = "dashed"];\n']),
	forall(member(Node, Nodes), graphviz_node(Node)),
	forall(member(Edge, Edges), graphviz_edge(Edge)),
	maplist(write, ['}\n']),
	flush_output,
	told.

graphviz_usage_example :- % with backtracking, generates the 10 internal representations of curves with n = 2, in Graphviz format.
	initial_state(S0),
	chain_moves(S0, 2, S1),
	graphviz_state(S1).

%===============================================================================
% MISC
%===============================================================================

:- dynamic(last_unique_id / 1).

unique_id(Y) :-
	last_unique_id(X)
	->
	(
		Y is X + 1,
		retract(last_unique_id(X)),
		assert(last_unique_id(Y))
	)
	;
	(
		Y = 1,
		assert(last_unique_id(Y))
	).
