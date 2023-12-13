package org.oeis;

public class Coord implements Comparable<Coord> {

	public int i;
	public int j;
	
	public Coord(int i, int j) {
		this.i = i;
		this.j = j;
	}

	@Override
	public int compareTo(Coord that) {
		int d;
		d = that.i - this.i;
		if (d != 0) return d;
		d = this.j - that.j;
		if (d != 0) return d;
		return 0;
	}
	
	
}
