package org.oeis;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.PrintStream;

/*
 * Java + Prolog program
 * 
 * Approach: brute force; in equation P = 1 + (x + y - 1) * Q, generate all possible Q's, based on nonnegativity constraints of the coefficients, in both P and Q.
 * 
 * The Java program actually generates a Prolog program. The Prolog program then uses a CLP technique to compute one term of the sequence.
 */

public class A279196_Nonnegativity {
	
	int N_MAX = 17;

	public static void main(String[] args) throws FileNotFoundException {
		PrintStream o = new PrintStream(new File("a279196.pl"));
		System.setOut(o);
		new A279196_Nonnegativity().main();
	}

	public void main() {
		generatePrologCode(N_MAX);
	}
	
	private void generatePrologCode(int n) {

		System.out.println("% Typical usage:");
		System.out.println("% :- time(a" + n + "(" + n + ", AN)).");
		System.out.println("% a" + n + "(N, AN) can also be used for any N < " + n + ".");

		System.out.println();

		System.out.println(":- dynamic count/1.");
		System.out.println();
		System.out.println("a" + n + "(N, AN) :-");
		System.out.println("\tretractall(count(_)),");
		System.out.println("\tassert(count(0)),");
		System.out.println("\tforall(possible_Q(N, _Q),");
		System.out.println("\t(");
		System.out.println("\t\tcount(C),");
		System.out.println("\t\tretract(count(C)),");
		System.out.println("\t\tCC is C + 1,");
		System.out.println("\t\tassert(count(CC))");
		System.out.println("\t)),");
		System.out.println("\tcount(AN).");

		System.out.println();
		
		System.out.println("possible_Q(N, Q) :-");
		for (int s = 0; s < n; s ++) {
			generatePrologCodeForAnAntidiagonal(s);
		}
		System.out.println("\t% Final");
		System.out.println("\tSum is " + sumString(triangular(n)) + ",");
		System.out.println("\tSum = N,");
		System.out.println("\tQ = " + listString(triangular(n)) + ".");
	}
	
	private String h(IJ ij) {
		return "H_" + ij.i + "_" + ij.j;
	}
	
	private String q(IJ ij) {
		return "Q_" + ij.i + "_" + ij.j;
	}
	
	private void generatePrologCodeForAnAntidiagonal(int s) {
		System.out.println("\t% Antidiagonal " + s);
		IJ ij = new IJ();
		for (ij.i = s; ij.i >= 0; ij.i --) {
			ij.j = s - ij.i;
			String min;
			if (ij.i == 0 && ij.j == 0) {
				min = "1";
			} else {
				min = "";
				if (ij.i > 0) {
					min += q(new IJ(ij.i - 1, ij.j));
				}
				if (ij.j > 0) {
					if (! min.isEmpty()) {
						min += " + ";
					}
					min += q(new IJ(ij.i, ij.j - 1));
				}
			}
			System.out.println("\t" + h(ij) + " is min(" + min + ", N - (" + sumString(cantor(ij)) + ")),");
			System.out.println("\tbetween(0, " + h(ij) + ", " + q(ij) + "),");
		}
		System.out.println();
	}
	
	private class IJ {
		public int i;
		public int j;
		public IJ() {}
		public IJ(int i, int j) { this.i = i; this.j = j; }
		@Override
		public String toString() {
			return "(" + i + "," + j + ")";
		}
	}
	
	private int triangular(int n) {
		return n * (n + 1) / 2;
	}
	private IJ cantor(int k) {
		int w = 0;
		while ((2 * w + 1) * (2 * w + 1) <= 8 * k + 1) {
			w ++;
		}
		w --;
		IJ ij = new IJ();
		ij.j = k - triangular(w);
		ij.i = w - ij.j;
		return ij;
	}
	
	private int cantor(IJ ij) {
		return triangular(ij.i + ij.j) + ij.j; 
	}

	private String sumString(int n) {
		if (n == 0) {
			return "0";
		}
		StringBuilder sb = new StringBuilder();
		for (int k = 0; k < n; k ++) {
			IJ ij = cantor(k);
			if (k > 0) {
				sb.append(" + ");
			}
			sb.append(q(ij));
		}
		return sb.toString();
	}
	
	private String listString(int n) {
		StringBuilder sb = new StringBuilder();
		sb.append("[");
		for (int k = 0; k < n; k ++) {
			IJ ij = cantor(k);
			if (k > 0) {
				sb.append(", ");
			}
			sb.append(q(ij));
		}
		sb.append("]");
		return sb.toString();
	}
}
