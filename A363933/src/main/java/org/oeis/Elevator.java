package org.oeis;

import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

public class Elevator {

	/*
	 * +---------i--------> i
	 * |         .
	 * |         .
	 * |         .
	 * j........01
	 * |        2
	 * V
	 * j
	 */
	public static Set<Polynomial> elevate(Polynomial p) {
		Set<Polynomial> s = new TreeSet<>();
		for (Map.Entry<Coord, Integer> e : p.coef.entrySet()) {
			Coord coord1 = e.getKey();
			Coord coord2 = new Coord(coord1.i - 1, coord1.j + 1);
			if (! p.coef.containsKey(coord2)) {
				continue;
			}
			Coord coord0 = new Coord(coord1.i - 1, coord1.j);
			Polynomial pp = p.clone();
			Polynomial.decrement(pp, coord1);
			Polynomial.decrement(pp, coord2);
			Polynomial.increment(pp, coord0);
			s.add(pp);
		}
		return s;
	}
	
}
