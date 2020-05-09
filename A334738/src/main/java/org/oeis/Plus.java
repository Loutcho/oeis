package org.oeis;

public class Plus implements Node {
	
	public Node a;
	public int b;
	public Plus(Node a, int b) {
		this.a = a;
		this.b = b;
	}

	@Override
	public Node abs() {
		if (b >= 0) {
			return this;
		} else {
			return new Abs(this);
		}
	}

	@Override
	public Node shift(int c) {
		if (b + c == 0) {
			return a;
		} else {
			return new Plus(a, b + c);
		}
	}
	
	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder();
		sb.append(a);
		sb.append(b > 0 ? "+" : "");
		sb.append(b);
		return sb.toString();
	}
	
	@Override
	public int compareTo(Node o) {
		return this.toString().compareTo(o.toString());
	}
}
