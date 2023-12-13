package org.oeis;

import java.util.Map;
import java.util.TreeMap;

public class Polynomial implements Comparable<Polynomial> {

	public Map<Coord, Integer> coef; // a coefficient is stored if and only if it is > 0.
	public Polynomial() {
		coef = new TreeMap<>();
	}
	
	@Override
	public Polynomial clone() {
		Polynomial p = new Polynomial();
		p.coef.putAll(this.coef);
		return p;
	}
	
	public int sumOfCoefficients() {
		int sum = 0;
		for (int c : coef.values()) {
			sum += c;
		}
		return sum;
	}

	@Override
	public int compareTo(Polynomial that) {
		return that.toString().compareTo(this.toString());
	}
	
	public String toString() {
		StringBuilder sb = new StringBuilder();
		int numTerm = 0;
		for (Map.Entry<Coord, Integer> e : coef.entrySet()) {
			numTerm ++;
			Coord k = e.getKey();
			Integer v = e.getValue();
			if (numTerm > 1) {
				sb.append(" + ");
			}

			String powerOfX = pow("x", k.i);
			String powerOfY = pow("y", k.j);
			String powers = powerOfX + powerOfY; 

			if (v > 1 || powers.isEmpty()) {
				sb.append(v);
			}
			sb.append(powers);
		}
		return sb.toString();
	}
	
	private String pow(String letter, int exponent) {
		switch (exponent) {
		case 0: return "";
		case 1: return letter;
		default: return letter + "^" + exponent;
		}
	}
	
	public static void decrement(Polynomial pp, Coord coord) {
		int coefficient = pp.coef.get(coord);
		if (coefficient == 1) {
			pp.coef.remove(coord);
		} else {
			pp.coef.put(coord, coefficient - 1);
		}
	}
	
	public static void increment(Polynomial pp, Coord coord) {
		Integer coef = (pp.coef.containsKey(coord)) ? pp.coef.get(coord) : 0;  
		pp.coef.put(coord, coef + 1);
	}
}
