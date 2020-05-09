package org.oeis;

public interface Node extends Comparable<Node> {
	public Node abs();
	public Node shift(int c);
}
