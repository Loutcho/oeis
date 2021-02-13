package org.oeis;

import java.util.SortedSet;
import java.util.TreeSet;

public class Pair implements Comparable<Pair> {

	private SortedSet<Point> points;
	
	public Pair(Point p1, Point p2) {
		points = new TreeSet<>();
		points.add(p1);
		points.add(p2);
	}

	public Direction getDirection() {
		Point[] tab = this.points.toArray(new Point[0]);
		return new Direction(tab[0].x - tab[1].x, tab[0].y - tab[1].y);
	}
	
	@Override
	public int compareTo(Pair that) {
		Point[] tab1 = this.points.toArray(new Point[0]);
		Point[] tab2 = that.points.toArray(new Point[0]);
		int d;
		d = tab1[0].compareTo(tab2[0]); if (d != 0) { return d; }
		d = tab1[1].compareTo(tab2[1]); if (d != 0) { return d; }
		return 0;
	}

	@Override
	public String toString() {
		Point[] tab = this.points.toArray(new Point[0]);
		return "{" + tab[0] + ", " + tab[1] + "}";
	}
}
