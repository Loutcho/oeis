unique_id(X) :- uuid(X).

% Abbreviated version. Risky.
%unique_id(X) :-
%	uuid(UUID),
%	atom_string(UUID, S),
%	split_string(S, "-", "", L),
%	nth1(5, L, X).

graphviz_node(Node) :-
	Node = node(id(Id), number(Number), prev(Prev), next(Next)),
	maplist(write, ['\t"', Id, '" [shape = "circle" style = "filled" fillcolor = "beige" label = "', Number, '"];\n']),
	maplist(write, ['\t"', Id, '" -> "', Prev, '" [style = "dotted" color = "red"];\n']),
	maplist(write, ['\t"', Id, '" -> "', Next, '" [pendwidth = "3" color = "green"];\n']).

graphviz_edge(Edge) :-
	Edge = edge(id(Id), prev(Prev), next(Next), partner(Partner)),
	%maplist(write, ['\t"', Id, '" [shape = "square" label = "" style = "filled" fillcolor = "skyblue"];\n']),
	maplist(write, ['\t"', Id, '" [shape = "point"];\n']),
	maplist(write, ['\t"', Id, '" -> "', Prev, '" [style = "dotted" color = "red"];\n']),
	maplist(write, ['\t"', Id, '" -> "', Next, '" [pendwidth = "3" color = "green"];\n']),
	(
		Partner = none
		->
			maplist(write, ['\t"none" [shape = "none" label = "&#8709;"];\n'])
		;
		true
	),
	maplist(write, ['\t"', Id, '" -> "', Partner, '" [style = "dashed" color = "blue"];\n']).

graphviz_state(State) :-
	tell('C:\\Users\\Luc\\Desktop\\dessinpl.gv'),
	State = state(
		nodes(Nodes),
		edges(Edges),
		seed(Seed)
	),
	maplist(write, ['digraph G {\n']),
	maplist(write, ['\tseed [shape = "none" label = "&#9765;"];\n']),
	maplist(write, ['\tseed -> "', Seed, '" [arrowhead = "none" style = "dashed"];\n']),
	forall(member(Node, Nodes), graphviz_node(Node)),
	forall(member(Edge, Edges), graphviz_edge(Edge)),
	maplist(write, ['}\n']),
	told.

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

legal_move(S, M) :-
	S = state(nodes(_), edges(Edges), _Seed),
	member(Edge, Edges),
	Edge = edge(_Id, _Next, _Prev, partner(Partner)),
	not(Partner = none),
	M = move(Edge).

make_move(S, M, SS) :-
	S = state(_Nodes, _Edges, seed(Seed)),
	M = move(Edge),
	Edge = edge(_Id, prev(Prev), next(Next), _Partner),
	(
		(Next = Seed, cross_left(S, SS))
	;
		(Prev = Seed, cross_right)
	;
		(not(Next = Seed), not(Prev = Seed), cross)
	).

node_upd_prev(NodeIn, NewPrev, NodeOut) :-
	NodeIn = node(Id, Number, prev(_), Next),
	NodeOut = node(Id, Number, prev(NewPrev), Next).
	
node_upd_next(NodeIn, NewNext, NodeOut) :-
	NodeIn = node(Id, Number, Prev, next(_)),
	NodeOut = node(Id, Number, Prev, next(NewNext)).

edge_upd_prev(EdgeIn, NewPrev, EdgeOut) :-
	EdgeIn = edge(Id, prev(_), Next, Partner),
	EdgeOut = edge(Id, prev(NewPrev), Next, Partner).

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

state_upd_edge(StateIn, NewEdge, StateOut) :-
	StateIn = state(Nodes, edges(Edges), Seed),
	NewEdge = edge(id(Id), _Prev, _Next, _Partner),
	OldEdge = edge(id(Id), _, _, _),
	select(OldEdge, Edges,  EdgesWithoutOldEdge),
	NewEdges = [NewEdge | EdgesWithoutOldEdge],
	StateOut = state(Nodes, edges(NewEdges), Seed).

state_add_edge(StateIn, Edge, StateOut) :-
	StateIn = state(Nodes, edges(Edges), Seed),
	StateOut = state(Nodes, edges([Edge | Edges]), Seed).

state_upd_seed(StateIn, NewSeed, StateOut) :-
	StateIn = state(Nodes, Edges, _OldSeed),
	StateOut = state(Nodes, Edges, seed(NewSeed)).

state_get_node(State, Id, Node) :-
	State = state(nodes(Nodes), _Edges, _Seed),
	Node = node(id(Id), _Number, _Prev, _Next),
	member(Node, Nodes).

state_get_edge(State, Id, Edge) :-
	State = state(_Nodes, edges(Edges), _Seed),
	Edge = edge(id(Id), _Prev, _Next, _Partner),
	member(Edge, Edges).

state_get_seed(State, Seed) :-
	State = state(_Nodes, _Edges, seed(Seed)).

cross_left(StateIn, StateOut) :-
	state_get_seed(StateIn, Seed),
	S1 = Seed, unique_id(S2), unique_id(S3), unique_id(S4), unique_id(TT),
	unique_id(S1_S2), unique_id(S2_TT), unique_id(TT_S3), unique_id(S4_S4),

	state_get_node(StateIn, Seed, node(id(Seed), number(M), prev(Q), next(R))),
	N is M + 1,

	Node_S1    = node(id(S1   ), number(M), prev(Q    ), next(S1_S2)                ),
	Edge_S1_S2 = edge(id(S1_S2),            prev(S1   ), next(S2   ), partner(S4_S4)),
	Node_S2    = node(id(S2   ), number(M), prev(S1_S2), next(S2_TT)                ),
	Edge_S2_TT = edge(id(S2_TT),            prev(S2   ), next(TT   ), partner(TT_S3)),
	Node_TT    = node(id(TT   ), number(N), prev(S2_TT), next(TT_S3)                ),
	Edge_TT_S3 = edge(id(TT_S3),            prev(TT   ), next(S3   ), partner(S2_TT)),
	Node_S3    = node(id(S3   ), number(M), prev(TT_S3), next(R    )                ),
	Node_S4    = node(id(S4   ), number(M), prev(S4_S4), next(S4_S4)                ),
	Edge_S4_S4 = edge(id(S4_S4),            prev(S4   ), next(S4   ), partner(S1_S2)),
	
	state_get_edge(StateIn, R, Edge_R),
	edge_upd_prev(Edge_R, S3, NewEdge_R),
	
	state_upd_node(StateIn , Node_S1   , State001),
	state_add_edge(State001, Edge_S1_S2, State002),
	state_add_node(State002, Node_S2   , State003),
	state_add_edge(State003, Edge_S2_TT, State004),
	state_add_node(State004, Node_TT   , State005),
	state_add_edge(State005, Edge_TT_S3, State006),
	state_upd_edge(State006, NewEdge_R , State007),
	state_add_node(State007, Node_S3   , State008),
	state_add_node(State008, Node_S4   , State009),
	state_add_edge(State009, Edge_S4_S4, State010),
	state_upd_seed(State010, TT, StateOut).
	

cross_right :-
	writeln(cross_right).

cross :-
	writeln(cross).

go :-
	initial_state(S0),
	cross_left(S0, S1),
	cross_left(S1, S2),
	cross_left(S2, S3),
	graphviz_state(S3).