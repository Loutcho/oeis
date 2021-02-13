package org.oeis;

public class Point implements Comparable<Point> {

	public int x;
	public int y;
	
	public Point(int x, int y) {
		this.x = x;
		this.y = y;
	}
	
	@Override
	public int compareTo(Point that) {
		int d;
		d = this.x - that.x; if (d != 0) { return d; }
		d = this.y - that.y; if (d != 0) { return d; }			
		return 0;
	}
	
	@Override
	public String toString() {
		return "(" + x + ", " + y + ")";
	}
}