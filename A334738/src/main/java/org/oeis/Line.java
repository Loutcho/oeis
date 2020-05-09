package org.oeis;

public class Line {
	public int slope;
	public int yIntercept;
	
	public Line(int slope, int yIntercept) {
		this.slope = slope;
		this.yIntercept = yIntercept;
	}
	
	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder();
		if (slope == -1) {
			sb.append("-");
		}
		sb.append("x");
		if (yIntercept == 0) {
			return sb.toString();
		}
		sb.append(yIntercept > 0 ? "+" : "-");
		sb.append(Math.abs(yIntercept));
		return sb.toString();
	}
	
	int root() {
		return -yIntercept / slope;
	}
	
	Line negate() {
		return new Line(-slope, -yIntercept);
	}
	
	Line shift(int c) {
		return new Line(slope, yIntercept + c);
	}
}
