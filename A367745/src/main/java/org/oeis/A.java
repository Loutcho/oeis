package org.oeis;

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Locale;

public class A {

	public static void main(String[] args) throws IOException {
		new A("C:\\Users\\Luc\\Desktop\\a367745.svg").run();
	}
	
	private String filename;
	private int n = 1;
	private final int N_MAX = 8;
	private final int WIDTH = 1360;
	private final int HEIGHT = 900;
	private BufferedWriter writer;
	
	private A(String filename) {
		this.filename = filename;
	}
	
	private void run() throws IOException {

		writer = new BufferedWriter(new FileWriter(filename));

		Locale.setDefault(new Locale("en", "US"));
		
		writer.write("<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>");
		writer.write("<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\" \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">");
		writer.write(String.format("<svg width=\"%d\" height=\"%d\" xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" style=\"background-color:lightgray\">", WIDTH, HEIGHT));
		
		List<String> l = Arrays.asList("1", "0");
		node(fx(0), fy(0), "1");
		node(fx(0), fy(1), "0");

		while (n <= N_MAX) {
			n ++;
			l = child(l);
		}
		
		line(0, 0, 1360, 0);
		line(0, 0, 0, 900);
		line(0, 900, 1360, 900);
		line(1360, 0, 1360, 900);
		
		writer.write("</svg>");
		
		writer.close();
	}
	
	private List<String> child(List<String> parent) throws IOException {
		List<String> ll = new ArrayList<>();
		int p = parent.size();
		String u = null, v = null;
		for (int j = 1; j < p; j ++) {
			int i = j - 1;
			u = parent.get(i);
			v = parent.get(j);
			ll.add(u);
			String uv = u + v;
			ll.add(uv);
			double xuv = n - 1;
			double yuv = ((double) (i + j) / 2.0) / (double) (p - 1);
			node(fx(xuv), fy(yuv), uv);
		}
		ll.add(v);
		return ll;
	}
	
	private void node(double cx, double cy, String text) throws IOException {
		double k = Math.pow(n, 2.0);
		text(cx, cy, 450.0 / k, "black", text);
	}
	
	private void point(double cx, double cy) throws IOException {
		circle(cx, cy, 2.5, "black", 1.0, "red");
	}
	private void circle(double cx, double cy, double r, String stroke, double strokeWidth, String fill) throws IOException {
		writer.write(
				String.format("<circle cx='%.15f' cy='%.15f' r='%.15f' stroke='%s' stroke-width='%.15f' fill='%s' />", cx, cy, r, stroke, strokeWidth, fill)
		);
	}
	private void line(double x1, double y1, double x2, double y2) throws IOException {
		writer.write(
				String.format("<line x1=\"%.15f\" y1=\"%.15f\" x2=\"%.15f\" y2=\"%.15f\" stroke='%s' stroke-width='%.15f' />", x1, y1, x2, y2, "rgb(127, 127, 255)", 3.0)
		);
	}
	private void text(double x, double y, double fontSize, String fillColor, String text) throws IOException {
		writer.write(
				String.format("<text x=\"%.15f\" y=\"%.15f\" font-size=\"%s\" fill=\"%s\" text-anchor=\"middle\" alignment-baseline=\"middle\">%s</text>", x, y, fontSize, fillColor, text)
		);
	}

	private static final double MARGE_X = 80.0;
	private static final double MARGE_Y = 20.0;
	private static final double COTE_X = 150.0;
	private static final double COTE_Y = 860.0;
	
	private static final double fx(double x) {
		return MARGE_X + COTE_X * x;
	}

	private static final double fy(double y) {
		return MARGE_Y + COTE_Y * y;
	}
}
