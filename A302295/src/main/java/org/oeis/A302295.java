package org.oeis;

/*
 * "Compressibilité" du nombre n
 * Recherche du motif répétable le plus court dans le développement de n en base 2
 * Cf. A302295
 * Ebauche de programme 
 */
public class A {
	
	private static final int N = 31;

	public static void main(String[] args) {
		for (int n = 1; n <= N; n ++) {
			int an = a(n);
			// System.out.println(an);
		}
	}
	
	private static int a(int n) {
		String s = Integer.toString(n, 2);
		System.out.println("n = " + n + " = [" + s + "]");
		int len = s.length();
		for (int w = 1; w <= len; w ++) {
			f(n, w, s, len);
		}
		System.out.println("=================");
		return 1;
	}
	
	private static void f(int n, int w, String s, int len) {
		String ss = s;
		int r = len % w; 
		if (r != 0) {
			ss = "0".repeat(w - r) + s;
		}
		String motif = ss.substring(0, w);
		int q = ss.length() / w;
		int i = 0;
		boolean b = true;
		while (i < q) {
			b &= ss.substring(i * w, (i + 1) * w).equals(motif);
			i ++;
		}
		System.out.println("w = " + w + " -> n = [" + ss + "], motif = [" + motif + "] " + b);
	}
}
