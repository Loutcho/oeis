package org.oeis;

public class Interval {
	public int min;
	public int max;
	
	public Interval(int min, int max) {
		this.min = min;
		this.max = max;
	}
	
	@Override
	public String toString() {
		String sMin = (min == Constant.NEGATIVE_INFINITY) ? "-oo" : "" + min;
		String sMax = (max == Constant.POSITIVE_INFINITY) ? "+oo" : "" + max;
		return "(" + sMin + ";" + sMax + ")";
	}
}
