package org.oeis;

import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;

public class ExplorationData {

	public Set<Polynomial> explored;
	public Map<Integer, Set<Polynomial>> byDistance;
	public Map<Integer, Set<Polynomial>> byEvalAt1;
	
	public ExplorationData() {
		explored = new TreeSet<>();
		byDistance = new TreeMap<>();
		byEvalAt1 = new TreeMap<>();
	}
	
	public void add(Polynomial p, int d) {
		explored.add(p);

		Set<Polynomial> atDistanceD = byDistance.get(d);
		if (atDistanceD == null) {
			atDistanceD = new TreeSet<>();
			byDistance.put(d, atDistanceD);
		}
		atDistanceD.add(p);

		int e = p.sumOfCoefficients();
		Set<Polynomial> equalToEAt1 = byEvalAt1.get(e);
		if (equalToEAt1 == null) {
			equalToEAt1 = new TreeSet<>();
			byEvalAt1.put(e, equalToEAt1);
		}
		equalToEAt1.add(p);
	}
}
