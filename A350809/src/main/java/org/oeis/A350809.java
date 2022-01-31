package org.oeis;

import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;

public class A350809 {

	private static final Integer N_MAX = 100;
	
	public static void main(String[] args) {
		Set<Integer> primes = new TreeSet<>();
		Map<Integer, Set<Integer>> state = new TreeMap<>();
		System.out.print(state.size() + ", ");
		for (Integer n = 2; n <= N_MAX; n++) {
			if (state.containsKey(n)) {
				composite(n, state);
			} else {
				primes.add(n);
				prime(n, state);
			}
			System.out.print(state.size() + ", ");
		}
		// System.out.print("\nThe primes <= " + N_MAX + " are: " + primes);
	}

	private static void composite(Integer c, Map<Integer, Set<Integer>> state) {
		Set<Integer> s = state.get(c);
		for (Integer p : s) {
			Integer cc = c + p;
			Set<Integer> ss = state.get(cc);
			if (ss == null) {
				ss = new TreeSet<>();
				state.put(cc, ss);
			}
			ss.add(p);
		}
		state.remove(c);
	}

	private static void prime(Integer p, Map<Integer, Set<Integer>> state) {
		Integer c = 2 * p;
		Set<Integer> s = new TreeSet<>();
		s.add(p);
		state.put(c, s);
	}
}
