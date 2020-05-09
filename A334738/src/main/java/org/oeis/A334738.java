package org.oeis;

import java.util.ArrayList;
import java.util.Set;
import java.util.TreeSet;

public class A334738 {

	private static final int N = 33;
	
	public static void main(String[] args) {
		Knowledge knowledge = Knowledge.init();
		int n = 0;
		System.out.print("" + knowledge.functionsByDistance.get(n).size() + ", ");
		n = 1;
		while (n <= N) {
			knowledge.functionsByDistance.put(n, new ArrayList<>());
			for (Function f : knowledge.functionsByDistance.get(n - 1)) {
				Set<Node> representations = knowledge.functions.get(f);
				for (Functor functor : Functor.values()) {
					apply(functor, f, representations, n, knowledge);
				}
			}
			System.out.print("" + knowledge.functionsByDistance.get(n).size() + ", ");
			n ++;
		}
		// System.out.print("\nKnowledge = " + knowledge);
	}
	
	private static void apply(Functor functor, Function f, Set<Node> representations, int n, Knowledge knowledge) {
		Function g = null;
		Set<Node> representationsOfG = new TreeSet<>();
		switch (functor) {
		case ABS:
			g = abs(f);
			for (Node node : representations) {
				representationsOfG.add(abs(node));
			}
			break;
		case MINUS_ONE:
			g = shift(-1, f);
			for (Node node : representations) {
				representationsOfG.add(shift(-1, node));
			}
			break;
		}
		if (! knowledge.functions.containsKey(g)) {
			knowledge.functions.put(g, representationsOfG);
			knowledge.functionsByDistance.get(n).add(g);
		} else {
			knowledge.functions.get(g).addAll(representationsOfG);
		}
	}
	
	private static Function shift(int c, Function f) {
		Function g = new Function();
		g.pieces = new ArrayList<>();
		for (Piece p : f.pieces) {
			Piece q = new Piece(p.intervalOfDefinition, p.line.shift(c));
			g.pieces.add(q);
		}
		return g;
	}
	
	private static Node shift(int c, Node node) {
		return node.shift(c);
	}
	
	private static Function abs(Function f) {
		Function g = new Function();
		g.pieces = new ArrayList<>();
		for (Piece p : f.pieces) {
			int m = p.line.slope;
			int r = p.line.root();
			int a = p.intervalOfDefinition.min;
			int b = p.intervalOfDefinition.max;
			if (r <= a) {
				Interval i = new Interval(a, b);
				Line l = (m == +1) ? p.line : p.line.negate(); 
				g.pieces.add(new Piece(i, l));
			}
			if (b <= r) {
				Interval i = new Interval(a, b);
				Line l = (m == +1) ? p.line.negate() : p.line; 
				g.pieces.add(new Piece(i, l));
			}
			if ((a < r) && (r < b)){
				Interval i1 = new Interval(a, r);
				Interval i2 = new Interval(r, b);
				Line l1 = null;
				Line l2 = null;
				if (m == +1) {
					l1 = p.line.negate();
					l2 = p.line;
				} else {
					l1 = p.line;
					l2 = p.line.negate();
				}
				g.pieces.add(new Piece(i1, l1));
				g.pieces.add(new Piece(i2, l2));
			}
		}
		return g;
	}
	
	private static Node abs(Node node) {
		return node.abs();
	}

}
