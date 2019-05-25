package org.oeis;

import java.io.FileWriter;
import java.io.IOException;
import java.util.Map;
import java.util.Set;
import java.util.SortedMap;
import java.util.SortedSet;
import java.util.TreeMap;
import java.util.TreeSet;

public class A306597 {
	
	public static void main(String[] args) { new A306597().bfile(101, 200); }

	private void data() {
		int n = 1; for (;;) { System.out.printf("%d, ", a(n ++)); }
	}

	private void bfile(int nMin, int nMax) {
		try (FileWriter out = new FileWriter("b306597.txt");) {
			for (int n = nMin; n <= nMax; n ++) {
				out.write(String.format("%d %d\n", n, a(n)));
				out.flush();
			}
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	private static int t(int n) { return n * (n + 1) / 2; }

	private int a(int n) {
		int tn = t(n);
		SortedMap<Integer, SortedSet<Interval>> m = init();
		for (int k = n; k >= 1; k --) {
			m = merge(evolve(tn, k, m));
		}
		return size(m.get(tn));
	}
	
	private SortedMap<Integer, SortedSet<Interval>> init() {
		SortedMap<Integer, SortedSet<Interval>> m = new TreeMap<>();
		SortedSet<Interval> set0 = new TreeSet<>();
		set0.add(new Interval(0, 0));
		m.put(0, set0);
		return m;
	}
	
	private SortedMap<Integer, SortedSet<Interval>> evolve(
			int tn, int k, SortedMap<Integer, SortedSet<Interval>> m) {
		int tk = t(k);
		SortedMap<Integer, SortedSet<Interval>> mm = new TreeMap<>();
		for (Map.Entry<Integer, SortedSet<Interval>> entry : m.entrySet()) {
			int s = entry.getKey();
			Set<Interval> setF = entry.getValue();
			int r = tn - s;
			int xmin = (k > 1) ? 0 : r;
			int xmax = r / tk;
			for (int x = xmin; x <= xmax; x ++) {
				int ss = s + x * tk;
				SortedSet<Interval> setFF = mm.get(ss); 
				if (setFF == null) {
					setFF = new TreeSet<>();
					mm.put(ss, setFF);
				}
				for (Interval f : setF) {
					Interval ff = f.shift(x * k);
					setFF.add(ff);
				}
			}
		}
		return mm;
	}
	
	private SortedMap<Integer, SortedSet<Interval>> merge(SortedMap<Integer, SortedSet<Interval>> m) {
		SortedMap<Integer, SortedSet<Interval>> mm = new TreeMap<>();
		for (Map.Entry<Integer, SortedSet<Interval>> e : m.entrySet()) {
			mm.put(e.getKey(), merge(e.getValue()));
		}
		return mm;
	}
	
	private SortedSet<Interval> merge(SortedSet<Interval> s) {
		int min = -1;
		int max = -1;
		SortedSet<Interval> ss = new TreeSet<>();
		for (Interval i : s) {
			if (min == -1) {
				min = i.min;
				max = i.max;
			} else if (i.min > max + 1) {
				ss.add(new Interval(min, max));
				min = i.min;
				max = i.max;
			} else if (min <= i.min && i.min <= max) {
				max = Math.max(max, i.max);
			} else if (i.min == max + 1) {
				max = i.max;
			} else {
				throw new IllegalStateException();
			}
		}
		ss.add(new Interval(min, max));
		return ss;
	}
	
	private int size(SortedSet<Interval> s) {
		int t = 0;
		for (Interval i : s) {
			t += i.max - i.min + 1;
		}
		return t;
	}
	
	private class Interval implements Comparable<Interval> {
		private int min;
		private int max;
		public Interval(int min, int max) {
			this.min = min;
			this.max = max;
		}
		public Interval shift(int d) {
			return new Interval(min + d, max + d);
		}
		@Override
		public int compareTo(Interval that) {
			int dMin = this.min - that.min;
			return (dMin != 0) ? dMin : this.max - that.max;
		}
	}
}
