package org.oeis;

import java.util.Deque;
import java.util.LinkedList;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;

public class A {

	public static void main(String[] args) {
		new A().run();
	}
	
	private A() {
	}
	
	private void run() {
		Polynomial p0 = new Polynomial();
		p0.coef.put(new Coord(0, 0), 1);
		breadthFirstSearch(p0);
	}

	private static final int SUM_MAX = 12;

	private void breadthFirstSearch(Polynomial root) {
		Deque<Polynomial> q = new LinkedList<>();
		Set<Polynomial> explored = new TreeSet<>();
		Map<Polynomial, Polynomial> parent = new TreeMap<>(); // clé a pour parent valeur
		// NB : explored et parent font un peu double emploi, car les clés dans parent == les éléments de explored
		explored.add(root);
		parent.put(root, null);
		q.add(root);
		while (! q.isEmpty()) {
			// System.out.println("size of explored = " + explored.size());
			// System.out.println("size of parent = " + parent.size());
			// System.out.println("size of queue = " + q.size());
			Polynomial v = q.remove();
			if (v.sumOfCoefficients() > SUM_MAX) {
				return;
			}
			// System.out.println(v.sumOfCoefficients() + " : " + v);
			Set<Polynomial> degraded = Degrader.degrade(v);
			Set<Polynomial> elevated = Elevator.elevate(v);
			/*
			Set<Polynomial> neighbours = new TreeSet<>();
			neighbours.addAll(degraded);
			neighbours.addAll(elevated);
			*/
			for (Polynomial w : degraded) {
				if (! explored.contains(w)) {
					explored.add(w);
					parent.put(w, v);
					q.addLast(w);
				}
			}
			for (Polynomial w : elevated) {
				if (! explored.contains(w)) {
					System.out.println(w.sumOfCoefficients() + " : " + w);
					explored.add(w);
					parent.put(w, v);
					q.addLast(w);
				}
			}
		}
	}
}
