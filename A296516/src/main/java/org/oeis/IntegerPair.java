package org.oeis;

public class IntegerPair {
	public Integer i;
	public Integer j;
	public IntegerPair(Integer i, Integer j) { this.i = i; this.j = j; }
	@Override
	public String toString() {
		return "(" + i + ", " + j + ")";
	}
}
