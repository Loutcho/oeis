package org.oeis;

public class Abs implements Node {
	public Node underAbs;
	public Abs(Node underAbs) {
		this.underAbs = underAbs;
	}

	@Override
	public Node abs() {
		return this;
	}

	@Override
	public Node shift(int c) {
		return new Plus(this, c);
	}
	
	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder();
		sb.append("|");
		sb.append(underAbs);
		sb.append("|");
		return sb.toString();
	}

	@Override
	public int compareTo(Node o) {
		return this.toString().compareTo(o.toString());
	}
}
