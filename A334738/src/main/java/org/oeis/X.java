package org.oeis;

public class X implements Node {

	@Override
	public Node abs() {
		return new Abs(this);
	}

	@Override
	public Node shift(int c) {
		return new Plus(this, c);
	}
	
	@Override
	public String toString() {
		return "x";
	}
	
	@Override
	public int compareTo(Node o) {
		return this.toString().compareTo(o.toString());
	}
}
