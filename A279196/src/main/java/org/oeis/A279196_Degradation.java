package org.oeis;

import java.util.Iterator;
import java.util.Map;
import java.util.Map.Entry;
import java.util.TreeMap;

/*
 * Java program
 * 
 * Approach: brute force; degrade 1, n-1 times.
 * 
 * The program generates the sequence. Memory consuming: use a big -Xmx JVM parameter, e.g. -Xmx12g
 */

public class A279196_Degradation {
	
	// 1, 1, 2, 5, 13, 36, 102, 295, 864, 2557, 7624, 22868, 68920, 208527, 632987, 1926752, 5878738, ...

	private static final int N_MAX = 5;
	
	public static void main(String[] args) { new A279196_Degradation().main(); }
	public void main() {
		Map<Config, Long> population = theConfigWithOnePointAt00();
		int n = 1;
		displayPopulation(n, population);
		while (n < N_MAX) {
			n ++;
			population = evolvePopulation(population);
			displayPopulation(n, population);
		}
	}
	
	private Map<Config, Long> theConfigWithOnePointAt00() {
		Map<Config, Long> population = new TreeMap<>();
		Config init = new Config();
		init.addPoint(new Point(0, 0), 1);
		population.put(init, 1L);
		return population;
	}
	
	private Map<Config, Long> evolvePopulation(Map<Config, Long> population) {
		Map<Config, Long> newPopulation = new TreeMap<>();
		for (Map.Entry<Config, Long> configEntry : population.entrySet()) {
			Config config = configEntry.getKey();
			Long configMultiplicity = configEntry.getValue();
			for (Map.Entry<Point, Integer> pointEntry : config.map.entrySet()) {
				Point p = pointEntry.getKey();
				Integer pointMultiplicity = pointEntry.getValue();
				Config configClone = config.clone();
				configClone.removePoint(p, 1);
				Point p1 = new Point(p.x + 1, p.y);
				Point p2 = new Point(p.x, p.y + 1);
				configClone.addPoint(p1, 1);
				configClone.addPoint(p2, 1);
				Long configNewMultiplicity = newPopulation.get(configClone);
				if (configNewMultiplicity == null) {
					configNewMultiplicity = 0L;
				}
				newPopulation.put(configClone, (long) (configNewMultiplicity + pointMultiplicity * configMultiplicity));
			}
		}
		return newPopulation;
	}
	
	private class Point implements Comparable<Point> {
		public int x, y;
		public Point(int x, int y) { this.x = x; this.y = y; }
		@Override
		public int compareTo(Point that) {
			int d;
			d = this.y - that.y; if (d != 0) { return d; }
			d = this.x - that.x; if (d != 0) { return d; }
			return 0;
		}
		@Override
		public String toString() {
			return "(" + x + "," + y + ")";
		}
	}
	
	private class Config implements Comparable<Config> {
		public Map<Point, Integer> map;
		public Config() { map = new TreeMap<>(); }
		@Override public Config clone() {
			Config that = new Config();
			that.map.putAll(this.map);
			return that;
		}
		public void addPoint(Point p, int multiplicity) {
			Integer i = map.get(p);
			if (i == null) {
				i = 0;
			}
			map.put(p, i + multiplicity);
		}
		public void removePoint(Point p, int multiplicity) {
			Integer i = map.get(p);
			if (i == null || i < multiplicity) {
				throw new IllegalStateException("Cannot remove");
			}
			int j = i - multiplicity;
			if (j > 0) {
				map.put(p, j);
			} else {
				map.remove(p);
			}
			
		}
		@Override
		public int compareTo(Config that) {
			int d;
			d = this.map.size() - that.map.size();
			if (d != 0) { return d; }
			Iterator<Entry<Point, Integer>> itThis = this.map.entrySet().iterator();
			Iterator<Entry<Point, Integer>> itThat = that.map.entrySet().iterator();
			while (itThis.hasNext() && itThat.hasNext()) {
				Entry<Point, Integer> entryThis = itThis.next();
				Entry<Point, Integer> entryThat = itThat.next();
				Point pointThis = entryThis.getKey();
				Integer multiplicityThis = entryThis.getValue();
				Point pointThat = entryThat.getKey();
				Integer multiplicityThat = entryThat.getValue();
				d = pointThis.compareTo(pointThat);
				if (d != 0) { return d; }
				d = multiplicityThis - multiplicityThat;
				if (d != 0) { return d; }
			}
			return 0;
		}
		@Override
		public String toString() {
			return map.toString();
		}

		public String toPolynomialForm() {
			StringBuilder sb = new StringBuilder();
			int termNumber = 0;
			for (Map.Entry<Point, Integer> entry : map.entrySet()) {
				termNumber ++;
				Point p = entry.getKey();
				Integer c = entry.getValue();
				if (termNumber > 1) {
					sb.append(" + ");					
				}

				String sc, sx, sy, sxy;

				switch (p.x) {
				case 0: sx = ""; break;
				case 1: sx = "x"; break;
				default: sx = "x^" + p.x; break;
				}
				switch (p.y) {
				case 0: sy = ""; break;
				case 1: sy = "y"; break;
				default: sy = "y^" + p.y; break;
				}
				if (sx.isEmpty() && sy.isEmpty()) {
					sc = c.toString();
				} else {
					sc = (c == 1) ? "" : (c + "*");
				}
				if (sx.isEmpty() || sy.isEmpty()) {
					sxy = sx + sy;
				} else {
					sxy = sx + "*" + sy;
				}
				sb.append(sc + sxy);
 			}
			return sb.toString();
		}
		
		public String toTikzCode() {
			StringBuilder sb = new StringBuilder();
			sb.append("\\begin{tikzpicture}[scale=0.6]\n");
			for (int i = 0; i <= 4; i ++) {
				sb.append("\\draw[black, line width=" + (i == 0 ? 3 : 1) + "] (" + i + ",0) -- (" + i + ",4.5);\n");
				sb.append("\\draw (" + i + ",0) node[below]{$" + i + "$};\n");
			}
			for (int j = 0; j <= 4; j ++) {
				sb.append("\\draw[black, line width=" + (j == 0 ? 3 : 1) + "] (0," + j + ") -- (4.5, " + j + ");\n");
				sb.append("\\draw (0," + j + ") node[left]{$" + j + "$};\n");
			}

			for (Map.Entry<Point, Integer> entry : map.entrySet()) {
				Point p = entry.getKey();
				Integer c = entry.getValue();
				sb.append("\\draw[color=gray, fill=lightgray, line width=2pt] (" + p.x + "," + p.y + ") circle (10pt);\n");
				sb.append("\\draw (" + p.x + "," + p.y + ") node{" + c + "} ;\n");
			}
			
			sb.append("\\end{tikzpicture}\n");

			return sb.toString();
		}
	}
	
	private void displayPopulation(int n, Map<Config, Long> population) {
		// commaSeparated(population);
		// or:
		// bFile(n, population);
		// or:
		// verbose(n, population);
		// or:
		// verbosePolynomialForm(n, population);
		// or:
		verboseTikzCode(n, population);
		// or:
		// checkSumsAreFactorials(population);
	}
	
	private void commaSeparated(Map<Config, Long> population) {
		System.out.print(population.size() + ", ");		
	}
	
	private void bFile(int n, Map<Config, Long> population) {
		System.out.println(n + " " + population.size());		
	}

	private void verbose(int n, Map<Config, Long> population) {
		System.out.println("======================== Population at stage " + n + " ============================");
		for (Map.Entry<Config, Long> entry : population.entrySet()) {
			Config config = entry.getKey();
			Long multiplicity = entry.getValue();
			System.out.println("" + multiplicity + " * " + config);
		}
	}
	
	private void verbosePolynomialForm(int n, Map<Config, Long> population) {
		System.out.println("======================== Population at stage " + n + " ============================");
		for (Map.Entry<Config, Long> entry : population.entrySet()) {
			Config config = entry.getKey();
			// Long multiplicity = entry.getValue();
			System.out.println(config.toPolynomialForm());
		}
	}
	
	private void verboseTikzCode(int n, Map<Config, Long> population) {
		System.out.println("Population at stage " + n + ":");
		for (Map.Entry<Config, Long> entry : population.entrySet()) {
			Config config = entry.getKey();
			// Long multiplicity = entry.getValue();
			System.out.print(config.toTikzCode());
		}
	}
	
	private void checkSumsAreFactorials(Map<Config, Long> population) {
		int sum = 0;
		for (Map.Entry<Config, Long> entry : population.entrySet()) {
			Long multiplicity = entry.getValue();
			sum += multiplicity;
		}
		System.out.print(sum + ", ");
	}
}
