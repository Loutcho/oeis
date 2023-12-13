package org.oeis;

import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

public class Degrader {

	public static Set<Polynomial> degrade(Polynomial p) {
		Set<Polynomial> s = new TreeSet<>();
		for (Map.Entry<Coord, Integer> e : p.coef.entrySet()) {
			Coord coord = e.getKey();
			Polynomial pp = p.clone();
			Polynomial.decrement(pp, coord);
			Polynomial.increment(pp, new Coord(coord.i + 1, coord.j));
			Polynomial.increment(pp, new Coord(coord.i, coord.j + 1));
			s.add(pp);
		}
		return s;
	}
}
