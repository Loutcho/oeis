package org.oeis;

import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;

public class A350809 {

	private static final int N_MAX = 100;
	
	public static void main(String[] args) {
		// Set<Integer> primes = new TreeSet<>();
		Map<Integer, Set<Integer>> frontier = new TreeMap<>();
		System.out.print("0, ");
		for (int n = 2; n <= N_MAX; n++) {
			if (! frontier.containsKey(n)) {
				// primes.add(n);
				Set<Integer> newBranch = new TreeSet<>();
				newBranch.add(n);
				frontier.put(2 * n, newBranch);
			} else {
				Set<Integer> branch = frontier.get(n);
				for (Integer p : branch) {
					Set<Integer> update = frontier.get(n + p);
					if (update == null) {
						update = new TreeSet<>();
						frontier.put(n + p, update);
					}
					update.add(p);
				}
				frontier.remove(n);
			}
			// System.out.print(frontier.size() + ", ");
			System.out.println(n + ":" + frontier);
		}
	}
}
