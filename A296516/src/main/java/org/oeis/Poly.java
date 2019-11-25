package org.oeis;

import java.util.Map;
import java.util.TreeMap;

public class Poly {
	public Map<Integer, IntegerPair> coefs;
	public Poly() { coefs = new TreeMap<>(); }
	public Poly(Integer k, Integer i, Integer j) {
		coefs = new TreeMap<>();
		consider(k, i, j);
	}
	public void consider(Integer k, Integer i, Integer j) {
		IntegerPair ip = coefs.get(k);
		if (ip == null) {
			coefs.put(k, new IntegerPair(i, j));
		} else {
			if (ip.i > i) { ip.i = i; }
			if (ip.j < j) { ip.j = j; }
		}
	}
	public static Poly add(Poly p, Poly q) {
		Poly pq = new Poly();
		for (Map.Entry<Integer, IntegerPair> e: p.coefs.entrySet()) {
			IntegerPair v = e.getValue();
			pq.consider(e.getKey(), v.i, v.j);
		}
		for (Map.Entry<Integer, IntegerPair> e: q.coefs.entrySet()) {
			IntegerPair v = e.getValue();
			pq.consider(e.getKey(), v.i, v.j);
		}
		return pq;
	}
	public static Poly mul(Poly p, Poly q) {
		Poly pq = new Poly();
		// A(m) * B(i, j) * A(n) * B(k, l) = A(m + n) * B(i + k, j + l) + A(|m - n|) * B(i + k + o, j + l + o)
		for (Map.Entry<Integer, IntegerPair> e: p.coefs.entrySet()) {
			for (Map.Entry<Integer, IntegerPair> f: q.coefs.entrySet()) {
				Integer m = e.getKey();
				IntegerPair v = e.getValue();
				Integer i = v.i;
				Integer j = v.j;
				Integer n = f.getKey();
				IntegerPair w = f.getValue();
				Integer k = w.i;
				Integer l = w.j;
				Integer o = Math.min(m, n);
				pq.consider(m + n, i + k, j + l);
				pq.consider(Math.abs(m - n), i + k + o, j + l + o);
			}
		}
		return pq;
	}
	@Override
	public String toString() {
		return coefs.toString();
	}
	
}
