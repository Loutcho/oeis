package org.oeis;

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

/*
 * We construct the lexicographically earliest sequence of 2D points with the following rules:
 * 
 * (i) the (x, y) coordinates are taken in the nonnegative integers.
 * (ii) The M(0) point is (0, 0).
 * (iii) For n > 0, if n is even then
 *      - M(n) has the same x coordinate as M(n-1),
 *      - for all k's less than n, M(n) has not the same y coordinate as M(k).
 * (iv) For n > 0, if n is odd then
 *      - M(n) has the same y coordinate as M(n-1),
 *      - for all k's less than n, M(n) has not the same x coordinate as M(k).
 * (v) No three points are aligned.
 * 
 * Then,
 *     A362977(n) is defined as the x coordinate of M(2n),
 *     A362978(n) is defined as the y coordinate of M(2n).
 */

public class A362977 {
	
	private static int N_MAX = 300;

	private List<Integer> lx = new ArrayList<>();
	private List<Integer> ly = new ArrayList<>();
	
	public static void main(String[] args) throws IOException { new A362977().run(); } private A362977() {} private void run() throws IOException {
		lx.add(0);
		ly.add(0);
		int n = 1;
		while (n <= N_MAX) {
			computePoint(n);
			n ++;
		}
		System.out.println("M-Sequence:");
		for (n = 0; n <= N_MAX; n ++) {
			System.out.print("(" + lx.get(n) + "," + ly.get(n) + "), ");
		}
		System.out.println();
		System.out.println("x-Sequence:");
		System.out.println(lx);
		System.out.println("y-Sequence:");
		System.out.println(ly);
		
		System.out.println("A362977:");
		for (n = 0; n <= N_MAX / 2; n ++) {
			System.out.print(a(n) + ", ");
		}
		System.out.println();
		System.out.println("A362978:");
		for (n = 0; n <= N_MAX / 2; n ++) {
			System.out.print(b(n) + ", ");
		}
		System.out.println();
		System.out.println("SVG: see a362977.svg");
		svg();
	}
	
	private void computePoint(int n) {
		int x, y;
		if (n % 2 == 0) {
			x = lx.get(n - 1);
			y = smallestY(n, x);
		} else {
			y = ly.get(n - 1);
			x = smallestX(n, y);
		}
		lx.add(x);
		ly.add(y);
	}
	
	private int smallestX(int n, int y) { int x = 0; while (! fitsX(n, x, y)) { x ++; } return x; }
	private int smallestY(int n, int x) { int y = 0; while (! fitsY(n, x, y)) { y ++; } return y; }
	
	private boolean fitsX(int n, int x, int y) {
		if (lx.contains(x)) { return false; }
		return noAlignment(n, x, y);
	}
	
	private boolean fitsY(int n, int x, int y) {
		if (ly.contains(y)) { return false; }
		return noAlignment(n, x, y);
	}
	
	private boolean noAlignment(int n, int xn, int yn) {
		for (int j = 1; j < n; j ++) {
			for (int i = 0; i < j; i ++) {
				int xi = lx.get(i);
				int yi = ly.get(i);
				int xj = lx.get(j);
				int yj = ly.get(j);
				int det = (xi * yj) + (xj * yn) + (xn * yi) - (yi * xj) - (yj * xn) - (yn * xi); 
				if (det == 0) { return false; }
			}
		}
		return true;
	}
	
	private int a(int n) { return lx.get(2 * n); }
	private int b(int n) { return ly.get(2 * n); }

	
	private static final int X_MAX = 100;
	private static final int Y_MAX = 50;
	
	private void svg() throws IOException {
		String filename = "a362977.svg";
		BufferedWriter writer = new BufferedWriter(new FileWriter(filename));
		
		writer.write("<?xml version='1.0'?>\n");
		writer.write("<svg xmlns='http://www.w3.org/2000/svg' width='1800' height='900' style='background-color:#101010' viewBox='-1 -49 100 50'>\n");
	
		// vertical grid
		for (int x = 0; x <= X_MAX; x ++) {
			String s = String.format("<line x1='%d' y1='0' x2='%d' y2='%d' stroke='gray' stroke-width='0.015' />\n", x, x, -Y_MAX);
			writer.write(s);
			String t = String.format("<text alignment-baseline='middle' text-anchor='middle' x='%d' y='+0.5' fill='white' font-size='0.5px' >%d</text>\n", x, x);
			writer.write(t);
		}

		// horizontal grid
		for (int y = 0; y <= Y_MAX; y ++) {
			String s = String.format("<line x1='0' y1='%d' x2='%d' y2='%d' stroke='gray' stroke-width='0.015' />\n", -y, X_MAX, -y);
			writer.write(s);
			String t = String.format("<text alignment-baseline='middle' text-anchor='middle' x='-0.5' y='%d' fill='white' font-size='0.5px' >%d</text>\n", -y, y);
			writer.write(t);
		}
		
		// staircase path of the M(n)
		for (int n = 0; n < N_MAX; n ++) {
			String s = String.format("<line x1='%d' y1='%d' x2='%d' y2='%d' stroke='#FF4444' stroke-width='0.25' />\n", lx.get(n), -ly.get(n), lx.get(n + 1), -ly.get(n + 1));
			writer.write(s);
		}
		
		// The M(n)
		for (int n = 0; n <= N_MAX; n ++) {
			String s = String.format("<circle cx='%d' cy='%d' r='0.4' stroke='#FF4444' stroke-width='0.01' fill='white' />\n", lx.get(n), -ly.get(n));
			writer.write(s);
			String t = String.format("<text alignment-baseline='middle' text-anchor='middle' x='%d' y='%d' fill='gray' font-size='0.6px' font-weight='bold' >%d</text>\n", lx.get(n), -ly.get(n), n);
			writer.write(t);
		}
		
		writer.write("</svg>\n");
		
		writer.close();
	}
}
