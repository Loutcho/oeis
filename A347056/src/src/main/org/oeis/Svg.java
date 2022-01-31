package org.oeis;

import java.io.BufferedOutputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.PrintStream;

public class Svg {
	
	public static void main(String[] args) throws FileNotFoundException {
		System.setOut(new PrintStream(new BufferedOutputStream(new FileOutputStream("a347056.svg"))));
		new Svg().run();
		System.out.flush();
	}

	private static long factorial(long n) {
		long f = 1;
		for (long i = 1; i <= n; i ++) {
			f *= i;
		}
		return f;
	}
	
	private static long t(long p, long n, long k) {
		if (n == 0 && p == 0) {
			return 1;
		}
		return ((k + p) * factorial(n + p - 1)) / (factorial(k) * factorial(n - k) * factorial(p)); 
	}
	
	private static final double X_CENTER = 900.0;
	private static final double Y_CENTER = 150.0;
	private static final double RADIAL_SPACING = 80.0;
	private static final double START_ANGLE = 0.97 * Math.PI;
	private static final double SECTOR_ANGLE = Math.PI / 6.0;
	private static final long FONT_SIZE = 12;
	
	private static final long N_MAX = 7;
	private static final long BIG_N = 50;
	private static final long P_MIN = 0;
	private static final long P_MAX = 4;
	
	private void line(double x1, double y1, double x2, double y2) {
		System.out.println("<line x1=\"" + x1 + "\" y1=\"" + y1 + "\" x2=\"" + x2 + "\" y2=\"" + y2 + "\" style=\"stroke:crimson;stroke-width:2\" />");
	}
	
	private void circle(double cx, double cy, double r) {
		System.out.println("\"<circle cx=\"" + cx + "\" cy=\"" + cy + "\" r=\"" + r + "\" stroke=\"lightgreen\" stroke-width=\"1\" fill=\"none\" />\";");
	}
	
	private void text(double x, double y, double adeg, String t, String color) {
		System.out.println("<text x=\"0\" y=\"0\" text-anchor=\"middle\" dominant-baseline=\"central\" style=\"font-family:Verdana;font-size:" + FONT_SIZE + "\" fill=\"" + color + "\" transform=\"translate(" + x + " " + y + ") rotate(" + adeg + ")\">" + t + "</text>");
	}
	
	private void sector_line(long p) {
		Location l1 = new Location(p, 0, 0);
		Location l2 = new Location(p, BIG_N, 0);
		line(l1.x, l1.y, l2.x, l2.y);
	}
	
	private void my_circle(long n) {
		circle(X_CENTER, Y_CENTER, n * RADIAL_SPACING);
		Location l = new Location(-0.50, n - 0.075, 0);
		text(l.x, l.y, l.adeg, "n = " + n, "green");
	}
	
	private void triangle_row(long p, long n) {
		for (long k = 0; k <= n; k ++) {
			double x, y;
			double arad = START_ANGLE - SECTOR_ANGLE * (p + k / (double) n); 
			double adeg = arad / Math.PI * 180.0 - 90.0;
			x = X_CENTER + RADIAL_SPACING * n * Math.cos(arad);
			y = Y_CENTER + RADIAL_SPACING * n * Math.sin(arad);
			long t = t(p, n, k);
			text(x, y, adeg, Long.toString(t), "white");
		}		
	}
	
	private void triangle_label(long p) {
		Location l = new Location(p, N_MAX + 1, (N_MAX + 1) / 2.0);
		text(l.x, l.y, l.adeg, "p = " + p, "white");
	}
	
	private void triangle(long p) {
		for (long n = 1; n <= N_MAX; n ++) {
			triangle_row(p, n);
		}
		triangle_label(p);
	}
	
	private void a_numbers() {
		Location l;

		l = new Location(0, N_MAX + 2, (N_MAX + 1) / 2.0);
		text(l.x, l.y, l.adeg, "A097805", "white");

		l = new Location(1, N_MAX + 2, (N_MAX + 1) / 2.0);
		text(l.x, l.y, l.adeg, "A103406", "white");
		
		l = new Location(2, N_MAX + 2, (N_MAX + 1) / 2.0);
		text(l.x, l.y, l.adeg, "A124932", "white");
		
		l = new Location(3, N_MAX + 2, (N_MAX + 1) / 2.0);
		text(l.x, l.y, l.adeg, "A347056", "yellow");
		
		l = new Location(5, N_MAX + 2, (N_MAX + 1) / 2.0);
		text(l.x, l.y, l.adeg, "etc.", "white");

	}
	
	private void run() {
		System.out.println(
				"<?xml version='1.0' standalone='no'?>\r\n" + 
				"<!DOCTYPE svg PUBLIC '-//W3C//DTD SVG 1.1//EN' 'http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd'>\r\n" + 
				"<svg xmlns='http://www.w3.org/2000/svg' version='1.1' width='1800.0' height='900.0' background-color='black'>\r\n" + 
				"	<rect fill='#000000' width='1800.0' height='900.0' />");
		for (long n = 1; n <= N_MAX; n ++) {
			my_circle(n);
		}
		for (long p = P_MIN; p <= P_MAX + 1; p ++) {
			sector_line(p);
		}
		for (long p = P_MIN; p <= P_MAX; p ++) {
			triangle(p);
		}
		text(X_CENTER, Y_CENTER, 0.0, "1", "white");
		a_numbers();
		System.out.println("</svg>");
	}
	
	private class Location {
		public double x;
		public double y;
		public double arad;
		public double adeg;
		public Location(double p, double n, double k) {
			arad = START_ANGLE - SECTOR_ANGLE * (p + k / (n == 0.0 ? 1.0 : n)); 
			adeg = arad / Math.PI * 180.0 - 90.0;
			x = X_CENTER + RADIAL_SPACING * n * Math.cos(arad);
			y = Y_CENTER + RADIAL_SPACING * n * Math.sin(arad);
		}
	}
}
