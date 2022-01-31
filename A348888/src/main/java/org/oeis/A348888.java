package org.oeis;

import java.util.Map;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeMap;
import java.util.TreeSet;

public class A348888 {

	private enum Mode {
		DATA,
		BFILE,
		TRIANGLE,
		SUPERSEEKER
	}
	
	public static void main(String[] args) {
		run(200, Mode.TRIANGLE);
		
	}
	
	private static void run(int nMax, Mode mode) {
		int i = 1;
		for (int n = 1; n <= nMax; n ++) {
			for (int k = 0; k < n; k ++) {
				int a = t(n, k);
				switch (mode) {
				case DATA:
					System.out.print(a + ", ");
					break;
				case SUPERSEEKER:
					System.out.print(a + " ");
					break;
				case BFILE:
					System.out.println(i + " " + a);
					break;
				case TRIANGLE:
					System.out.print(String.format("%2d ", a));
					break;
				}
				i ++;
			}
			if (mode == Mode.TRIANGLE) {
				System.out.println();
			}
		}
	}
	
	private static int t(int n, int k) {
		SortedSet<Integer> e = range(0, n - 1);
		Map<Integer, Set<Integer>> orb = new TreeMap<>();
		Map<Integer, Integer> rep = new TreeMap<>();

		while (! e.isEmpty()) {
			Integer x = takeFirst(e);
			Set<Integer> o = new TreeSet<>();
			Integer xx = x;
			while (xx != null) {
				o.add(xx);
				xx = (k * xx) % n;
				if (rep.containsKey(xx)) {
					mergeOrbits(xx, orb, o, rep);
					xx = null;
				} else if (o.contains(xx)) {
					registerOrbit(orb, rep, x, o);
					xx = null;
				} else {
					e.remove(xx);
				}
			}
		}
		int a = orb.size();
		return a;
	}
	
	private static SortedSet<Integer> range(int min, int max) {
		SortedSet<Integer> e = new TreeSet<>();
		for (int i = min; i <= max; i ++) {
			e.add(i);
		}		
		return e;
	}
	
	private static Integer takeFirst(SortedSet<Integer> e) {
		Integer x = e.first();
		e.remove(x);
		return x;
	}
	
	private static void mergeOrbits(Integer xx, Map<Integer, Set<Integer>> orb, Set<Integer> o, Map<Integer, Integer> rep) {
		Integer r = rep.get(xx);
		Set<Integer> orbit = orb.get(r);
		orbit.addAll(o);
		for(Integer i : o) {
			rep.put(i, r);
		}		
	}
	
	private static void registerOrbit(Map<Integer, Set<Integer>> orb, Map<Integer, Integer> rep, Integer x,	Set<Integer> o) {
		orb.put(x, o);
		for(Integer i : o) {
			rep.put(i, x);
		}
	}
}
