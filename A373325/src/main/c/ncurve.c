#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct node t_node;
typedef struct edge t_edge;
typedef struct state t_state;

struct node
{
	char name[10 + 1];
	int number;
	t_edge* prev;
	t_edge* next;
};

struct edge
{
	char name[10 + 1];
	t_node* prev;
	t_node* next;
	t_edge* partner;
};

struct state
{
	t_node* seed;
	int nb_nodes;
	t_node* nodes[10000];
	int nb_edges;
	t_edge* edges[10000];
};

static t_node* next_node(t_node* node) { return node -> next -> next; }
static t_node* prev_node(t_node* node) { return node -> prev -> prev; }
static t_edge* next_edge(t_edge* edge) { return edge -> next -> next; }
static t_edge* prev_edge(t_edge* edge) { return edge -> prev -> prev; }

static t_node* new_node(t_state* state, int number, char* name)
{
	const int sz = sizeof(t_node);
	t_node* n = (t_node*) malloc(sz);
	memset(n, 0x0, sz);
	n -> number = number;
	strcpy(n -> name, name);
	state -> nodes[state -> nb_nodes] = n;
	state -> nb_nodes++;
	return n;
}

static t_edge* new_edge(t_state* state, char* name)
{
	const int sz = sizeof(t_edge);
	t_edge *e = (t_edge*) malloc(sz);
	memset(e, 0x0, sz);
	strcpy(e -> name, name);
	state -> edges[state -> nb_edges] = e;
	state -> nb_edges++;
	return e;
}

static void free_node(t_node* node)
{
	node -> next = NULL;
	node -> prev = NULL;
}

static void free_edge(t_edge* edge)
{
	edge -> next = NULL;
	edge -> prev = NULL;
	edge -> partner = NULL;
}

static t_state* make_initial_state()
{
	t_state* state = (t_state*) malloc(sizeof(t_state));
	memset(state, 0x0, sizeof(state));
	state -> nb_nodes = 0;
	state -> nb_edges = 0;

	t_node* R = new_node(state, 0, "R");
	t_node* L = new_node(state, 0, "L");
	t_node* E = new_node(state, 1, "E");
	t_edge* er = new_edge(state, "er");
	t_edge* rl = new_edge(state, "rl");
	t_edge* le = new_edge(state, "le");

	R -> prev = er;
	R -> next = rl;

	rl -> prev = R;
	rl -> next = L;
	rl -> partner = NULL;

	L -> prev = rl;
	L -> next = le;

	le -> prev = L;
	le -> next = E;
	le -> partner = er;

	E -> prev = le;
	E -> next = er;

	er -> prev = E;
	er -> next = R;
	er -> partner = le;

	state -> seed = E;

	return state;
}

static int get_number_of_moves(t_state* state)
{
	int n = 0;
	t_node* current_node = state -> seed;
	t_edge* current_edge = current_node -> next;
	t_edge* start_edge = current_edge;
	do
	{
		if (current_edge -> partner != NULL)
		{
			n ++;
		}
		current_node = current_edge -> next;
		current_edge = current_node -> next;
	}
	while (current_edge != start_edge);
	return n;
}


static t_edge* get_move(t_state* state, int n)
{
	t_edge* edge = ((n > 0) ? (state -> seed -> next) : (state -> seed -> prev));
	if ((n == 1) || (n == -1))
	{
		return edge;
	}
	int k = 1;
	do
	{
		edge = ((n > 0) ? next_edge(edge) : prev_edge(edge));
		if (edge -> partner != NULL)
		{
			k = k + ((n > 0) ? (+1) : (-1));
		}
	}
	while (k != n);
	return edge;
}

static void display_node(t_node* node)
{
	printf("(%x:%d)", node, node -> number);
}

static void display_edge(t_edge* edge)
{
	printf("[%x:%x->%x:%x]", edge, edge -> prev, edge -> next, edge -> partner);
}

static void display_state(t_state* state)
{
	for (int i = 0; i < state -> nb_nodes; i ++)
	{
		display_node(state -> nodes[i]);
	}
	for (int i = 0; i < state -> nb_edges; i ++)
	{
		display_edge(state -> edges[i]);
	}
}

#define DEBUG 0

static void graphviz_node(t_node* node)
{
	if (node -> next == NULL && node -> prev == NULL) return;
	if (DEBUG == 0)
	{
		printf("\t\"%x\" [label = \"%d\" shape = \"circle\" style = \"filled\" fillcolor = \"skyblue\"];\n", node, node -> number);
	}
	else
	{
		printf("\t\"%x\" [label = \"%x\n%s\n%d\" shape = \"box\" style = \"rounded,filled\" fillcolor = \"skyblue\"];\n", node, node, node -> name, node -> number);
	}
	
	if (node -> next != NULL)
	{
		printf("\t\"%x\" -> \"%x\" [color = \"green\" penwidth = \"3\"];\n", node, node -> next);
	}

	if (node -> prev != NULL)
	{
		printf("\t\"%x\" -> \"%x\" [color = \"red\"];\n", node, node -> prev);
	}
}

static void graphviz_edge(t_edge* edge)
{
	if (edge -> next == NULL && edge -> prev == NULL) return;

	if (DEBUG == 0)
	{
		printf("\t\"%x\" [label = \"\" shape = \"square\" fixedsize = \"true\" width = \"0.3\" height = \"0.3\" style = \"filled\" fillcolor = \"floralwhite\"];\n", edge);
	}
	else
	{
		printf("\t\"%x\" [label = \"%x\n%s\n%x\" shape = \"box\" style = \"filled\" fillcolor = \"floralwhite\"];\n", edge, edge, edge -> name, edge -> partner);
	}

	if (edge -> next != NULL)
	{
		printf("\t\"%x\" -> \"%x\" [color = \"green\" penwidth = \"3\"];\n", edge, edge -> next);
	}

	if (edge -> prev != NULL)
	{
		printf("\t\"%x\" -> \"%x\" [color = \"red\"];\n", edge, edge -> prev);
	}

	if (edge -> partner != NULL)
	{
		printf("\t\"%x\" -> \"%x\" [style = \"dotted\" arrowhead = \"none\" color = \"blue\" constraint = \"false\"];\n", edge, edge -> partner);
	}
}

static void graphviz_state(t_state* state)
{
	printf("\ndigraph G {\n");
	// printf("\tepsilon = \"0.000001\";\n");
	printf("\trepulsiveforce = \"2.000001\";\n");

	for (int i = 0; i < state -> nb_nodes; i ++)
	{
		graphviz_node(state -> nodes[i]);
	}
	for (int i = 0; i < state -> nb_edges; i ++)
	{
		graphviz_edge(state -> edges[i]);
	}

	printf("}\n");
}

/*
 *
 *                                            ┌─────────────────────────────────────────────────────────┐
 *                                            │                                                         │
 *                                            │                                                         │
 *                                            │                       ┌──────────────▷[s23]──┐          │
 *                                            │                       │                 ┊    │          │
 *   ┌───────────────────────────────┐        │                       │         ┌─────[s44]  │          │
 *   │                               │        │       ┌────▷[ts2]──▷(S 2)     (S 4)─────┘    │          │
 *   │                               │        │     ( T )                                    │          │
 *   │          ┌─▷(S 1)──┐          │        │       └─────[s1t]◁──(S 1)     (S 3)◁─────────┘          │
 *   │          │         │          │        │                       △         │                       │
 *   │          │         │          │        │                       │         │                       │
 *   │          │         ▽          │  ===>  │                       │         ▽                       │
 *   │        [   ]┈┈┈┈┈[ r ]        │        │                     [   ]┈┈┈┈┈[ r ]                     │
 *   │          △         │          │        │                       △         │                       │
 *   │          │         │          │        │                       │         │                       │
 *   │          │         ▽          │        │                       │         ▽                       │
 *   └───────▷(   )     (   )────────┘        └────────────────────▷(   )     (   )─────────────────────┘
 */
static void cross_right(t_state* state)
{
	int n = state -> seed -> number;

	t_edge* r = state -> seed -> next;
	t_node* S1 = state -> seed;
	t_node* S2 = new_node(state, n, "S2");
	t_node* S3 = new_node(state, n, "S3");
	t_node* S4 = new_node(state, n, "S4");
	t_node* T = new_node(state, n + 1, "T");
	t_edge* s1t = new_edge(state, "s1t");
	t_edge* ts2 = new_edge(state, "ts2");
	t_edge* s23 = new_edge(state, "s23");
	t_edge* s44 = new_edge(state, "s44");

	// S1 -> prev : unchanged
	S1 -> next = s1t;
	strcpy(S1 -> name, "S1");

	s1t -> prev = S1;
	s1t -> next = T;
	s1t -> partner = ts2;

	T -> prev = s1t;
	T -> next = ts2;

	ts2 -> prev = T;
	ts2 -> next = S2;
	ts2 -> partner = s1t;

	S2 -> prev = ts2;
	S2 -> next = s23;

	s23 -> prev = S2;
	s23 -> next = S3;
	s23 -> partner = s44;

	S3 -> prev = s23;
	S3 -> next = r;
	r -> prev = S3;

	S4 -> prev = s44;
	S4 -> next = s44;

	s44 -> prev = S4;
	s44 -> next = S4;
	s44 -> partner = s23;

	state -> seed = T;
}

/*
 *
 *                                            ┌─────────────────────────────────────────────────────────┐
 *                                            │                                                         │
 *                                            │                                                         │
 *                                            │          ┌───▷[s12]─────────────┐                       │
 *                                            │          │      ┊               │                       │
 *   ┌───────────────────────────────┐        │          │    [s44]◁──┐         ▽                       │
 *   │                               │        │          │      └──▷(S 4)     (S 2)──▷[s2t]─────┐       │
 *   │                               │        │          │                              ┊     ( T )     │
 *   │          ┌─▷(S 1)──┐          │        │          └──────────(S 1)     (S 3)◁──[ts3]◁────┘       │
 *   │          │         │          │        │                       △         │                       │
 *   │          │         │          │        │                       │         │                       │
 *   │          │         ▽          │  ===>  │                       │         ▽                       │
 *   │        [   ]┈┈┈┈┈[ r ]        │        │                     [   ]┈┈┈┈┈[ r ]                     │
 *   │          △         │          │        │                       △         │                       │
 *   │          │         │          │        │                       │         │                       │
 *   │          │         ▽          │        │                       │         ▽                       │
 *   └───────▷(   )     (   )────────┘        └────────────────────▷(   )     (   )─────────────────────┘
 */
static void cross_left(t_state* state)
{
	int n = state -> seed -> number;

	t_edge* r = state -> seed -> next;
	t_node* S1 = state -> seed;
	t_node* S2 = new_node(state, n, "S2");
	t_node* S3 = new_node(state, n, "S3");
	t_node* S4 = new_node(state, n, "S4");
	t_node* T = new_node(state, n + 1, "T");
	t_edge* s12 = new_edge(state, "s12");
	t_edge* s2t = new_edge(state, "s2t");
	t_edge* ts3 = new_edge(state, "ts3");
	t_edge* s44 = new_edge(state, "s44");

	// S1 -> prev : unchanged
	S1 -> next = s12;
	strcpy(S1 -> name, "S1");

	s12 -> prev = S1;
	s12 -> next = S2;
	s12 -> partner = s44;

	S2 -> prev = s12;
	S2 -> next = s2t;

	s2t -> prev = S2;
	s2t -> next = T;
	s2t -> partner = ts3;

	T -> prev = s2t;
	T -> next = ts3;

	ts3 -> prev = T;
	ts3 -> next = S3;
	ts3 -> partner = s2t;

	S3 -> prev = ts3;
	S3 -> next = r;
	r -> prev = S3;

	S4 -> prev = s44;
	S4 -> next = s44;

	s44 -> prev = S4;
	s44 -> next = S4;
	s44 -> partner = s12;

	state -> seed = T;
}

/*
 *   ┌───────────────────────────────┐             ┌─────────────────────────────────────────┐
 *   │                               │             │                                         │
 *   │                               │             │                                         │
 *   │                               │             │               ┌─▷( F )──┐               │
 *   │                               │             │               │         │               │
 *   │                               │             │               │         │               │
 *   │                               │             │               │         ▽               │
 *   │                               │             │             [ u ]┈┈┈┈┈[ v ]             │
 *   │                               │             │               △         │               │
 *   │                               │             │               │         │               │
 *   ▽                               │             ▽               │         ▽               │
 * (B 2)──────────▷[w 2]──────────▷(A 2)         (B 2)──▷[y y]──▷(E 3)     (E 4)──▷[x x]──▷(A 2)
 *                   ┊                                     ┊                         ┊
 *                   ┊                    ===>             ┊                         ┊
 *                   ┊                                     ┊                         ┊
 * (B 1)◁──────────[w 1]◁──────────(A 1)         (B 1)◁──[ y ]◁──(E 1)     (E 2)◁──[ x ]◁──(A 1)
 *   │                               △             │               △         │               │
 *   │                               │             │               │         │               │
 *   │          ┌─▷( E )──┐          │             │               │         │               │
 *   │          │         │          │             │               │         │               │
 *   │          │         │          │             │               │         │               │
 *   │          │         ▽          │             │               │         ▽               │
 *   │        [ c ]┈┈┈┈┈[ d ]        │             │             [ c ]┈┈┈┈┈[ d ]             │
 *   │          △         │          │             │               △         │               │
 *   │          │         │          │             │               │         │               │
 *   │          │         ▽          │             │               │         ▽               │
 *   └───────▷(   )     (   )────────┘             └────────────▷(   )     (   )─────────────┘
 */
static void cross(t_state* state, t_edge *edge)
{
	int n = state -> seed -> number;
	t_node* E = state -> seed;
	t_edge* c = E -> prev;
	t_edge* d = E -> next;
	t_edge* w1 = edge;
	t_edge* w2 = w1 -> partner;
	t_node* B1 = w1 -> next;
	t_node* A1 = w1 -> prev;
	t_node* A2 = w2 -> next;
	t_node* B2 = w2 -> prev;
	t_node* E1 = new_node(state, n, "E1");
	t_node* E2 = new_node(state, n, "E2");
	t_node* E3 = new_node(state, n, "E3");
	t_node* E4 = new_node(state, n, "E4");
	t_node* F = new_node(state, n + 1, "F");
	t_edge* x = new_edge(state, "x");
	t_edge* y = new_edge(state, "y");
	t_edge* xx = new_edge(state, "xx");
	t_edge* yy = new_edge(state, "yy");
	t_edge* u = new_edge(state, "u");
	t_edge* v = new_edge(state, "v");

	strcpy(E -> name, "E");
	strcpy(c -> name, "c");
	strcpy(d -> name, "d");
	strcpy(w1 -> name, "w1");
	strcpy(w2 -> name, "w2");
	strcpy(B1 -> name, "B1");
	strcpy(A1 -> name, "A1");
	strcpy(A2 -> name, "A2");
	strcpy(B2 -> name, "B2");

	E1 -> next = y;
	E2 -> prev = x;
	E3 -> prev = yy; E3 -> next = u;
	E4 -> prev = v; E4 -> next = xx;
	F -> prev = u; F -> next = v;

	u -> prev = E3;
	u -> next = F;
	u -> partner = v;
	v -> prev = F;
	v -> next = E4;
	v -> partner = u;

	x -> partner = xx;
	y -> partner = yy;
	xx -> partner = x;
	yy -> partner = y;

	x -> next = E2;
	y -> prev = E1;
	xx -> prev = E4;
	yy -> next = E3;

	A1 -> next = x;
	x -> prev = A1;
	B1 -> prev = y;
	y -> next = B1;

	xx -> next = A2;
	A2 -> prev = xx;
	yy -> prev = B2;
	B2 -> next = yy;

	E1 -> prev = c;
	c -> next = E1; E -> prev = NULL;
	E2 -> next = d;
	d -> prev = E2; E -> next = NULL;

	free_node(E);
	free_edge(w1);
	free_edge(w2);

	state -> seed = F;
}

static void make_move_edge(t_state* state, t_edge *edge)
{
	if (state -> seed -> next == edge)
	{
		cross_right(state);
		return;
	}

	if (edge -> next == state -> seed)
	{
		cross_left(state);
		return;
	}

	cross(state, edge);
}

static void make_move_int(t_state* state, int n)
{
	t_edge* edge = get_move(state, n);
	make_move_edge(state, edge);
}


static void display_available_moves(t_state* state)
{
	int n = get_number_of_moves(state);
	printf("// There are %d available moves:\n", n);
	for (int k = 1; k <= n; k ++)
	{
		printf("// move number %d = ", k);
		display_edge(get_move(state, k));
		printf("\n");
	}
}

int main(int argc, char *argv[])
{
	t_state* state = make_initial_state();
	for (int i = 1; i < argc; i ++)
	{
		int move_number = atoi(argv[i]);
		make_move_int(state, move_number);
	}
	int n = get_number_of_moves(state);
	printf("%d\n", n);
}
