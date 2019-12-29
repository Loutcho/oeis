package org.oeis;

import java.util.Set;
import java.util.TreeSet;

/*
 * This program generates (almost) A123663.
 * The only difference is in the offset:
 * - in the OEIS, a(1) = 0, a(2) = 1, a(3) = 2, a(4) = 4, ...
 * - here,                  a(1) = 1, a(2) = 2, a(3) = 4, ...
 *
 * Reason: the algorithm of this program is based on the "pixellized hyperbola" interpretation of a(n).
 */

public class A123663 {

	public static void main(String[] args) {
		int n = 1;
		Set<Point> set = new TreeSet<>();
		set.add(new Point(1, 1));
		out(n, set.size());
		n ++;
		while (n <= 10000) {
			set = evolve(n, set);
			out(n, set.size());
			n ++;
		}
	}

	private static Set<Point> evolve(int n, Set<Point> set) {
		Set<Point> newSet = new TreeSet<>();
		for (Point p: set) {
			int xxy = n - (p.x + 1) * p.y;
			int xyy = n - p.x * (p.y + 1);
			if (xxy == 0) {
				newSet.add(new Point(p.x + 1, p.y));
			}
			if (xyy == 0) {
				newSet.add(new Point(p.x, p.y + 1));
			}
			if ((xxy < 0) || (xyy < 0)) {
				newSet.add(p);
			}
		}
		return newSet;
	}

	private static class Point implements Comparable<Point> {
		public int x, y;
		public Point(int x, int y) { this.x = x; this.y = y; }
		public String toString() { return "(" + x + "," + y + ")"; }
		@Override
		public int compareTo(Point that) {
			int d = this.x - that.x;
			if (d != 0) return d;
			return this.y - that.y;
		}
	}

	private static void out(int n, int a) {
		System.out.printf("%d %d\n", n, a);
	}
}
