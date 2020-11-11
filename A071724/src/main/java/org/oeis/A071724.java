package org.oeis;

import java.util.Arrays;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;

public class A071724 {
	
	private static final int N = 10;

	public static void main(String[] args) {
		A071724 a = new A071724(false);
		a.run();
	}
	
	boolean graphviz;
	private Set<Partition> allPartitionsSoFar = new TreeSet<>();
	private Set<Partition> newPartitions;
	private Map<Integer, Set<Partition>> map = new TreeMap<>();
	
	private A071724(boolean graphviz) {
		this.graphviz = graphviz;
	}
	
	private void run() {

		Partition p0 = new Partition();
		allPartitionsSoFar.add(p0);
		TreeSet<Partition> s0 = new TreeSet<>();
		s0.add(p0);
		map.put(0, s0);
		
		if (graphviz) {
			System.out.println("subgraph cluster_0{");
			System.out.println("\"0\";");
			System.out.println("}");
		}
		
		for (int n = 1; n <= N; n ++) {
			if (graphviz) {
				System.out.println("subgraph cluster_" + n + "{");
			}
			newPartitions = new TreeSet<>();
			for (int k = 1; k <= n; k ++) {
				for (Partition p : allPartitionsSoFar) {
					Partition pk = new Partition(p, k);
					boolean isNew = consider(pk, n);
					if (isNew) {
						if (graphviz) {
							System.out.println("\"" + pk + "\";");
							//System.out.println("\"" + p + "\" -> \"" + pk + "\";");
						}
					}
				}
			}
			allPartitionsSoFar.addAll(newPartitions);
			if (graphviz) {
				System.out.println("}");
			}
		}
		for (int n = 0; n <= N; n ++) {
			System.out.printf("%d, ", map.get(n).size());
		}
		System.out.println();
	}
	
	private boolean consider(Partition p, int depth) {
		if (allPartitionsSoFar.contains(p)) {
			return false;
		}
		newPartitions.add(p);
		Set<Partition> s = map.get(depth);
		if (s == null) {
			s = new TreeSet<>();
			map.put(depth, s);
		}
		s.add(p);
		return true;
	}
	
	private class Partition implements Comparable<Partition> {
		
		private int[] part;
		
		public Partition() {
			part = new int[0];
		}
		
		public Partition(Partition p, int n) {
			int l = p.part.length;
			part = new int[l + 1];
			for (int i = 0; i < l; i ++) {
				part[i] = p.part[i];
			}
			part[l] = n;
			Arrays.sort(part);
		}
		
		@Override
		public String toString() {
			if (part.length == 0) {
				return "0";
			}
			StringBuilder sb = new StringBuilder();
			for (int i = part.length - 1; i >= 0; i --) {
				sb.append(part[i]);
				if (i > 0) {
					sb.append("+");
				}
			}
			return sb.toString();
		}

		@Override
		public int compareTo(Partition that) {
			return this.toString().compareTo(that.toString());
		}
	}
}
