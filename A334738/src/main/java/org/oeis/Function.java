package org.oeis;

import java.util.ArrayList;
import java.util.List;

import static org.oeis.Constant.NEGATIVE_INFINITY;
import static org.oeis.Constant.POSITIVE_INFINITY;

public class Function implements Comparable<Function> {
	public List<Piece> pieces;
	
	public static final Function x;
	
	static {
		x = new Function();
		x.pieces = new ArrayList<>();
		x.pieces.add(new Piece(new Interval(NEGATIVE_INFINITY, POSITIVE_INFINITY), new Line(1, 0)));
	}

	@Override
	public int compareTo(Function that) {
		int dn = this.pieces.size() - that.pieces.size(); 
		if (dn != 0) {
			return dn;
		}
		int n = this.pieces.size();
		for (int i = 0; i < n; i ++) {
			Piece p = this.pieces.get(i);
			Piece q = that.pieces.get(i);
			int d;
			d = p.intervalOfDefinition.min - q.intervalOfDefinition.min;
			if (d != 0) {
				return d;
			}
			d = p.intervalOfDefinition.max - q.intervalOfDefinition.max;
			if (d != 0) {
				return d;
			}
			d = p.line.slope - q.line.slope;
			if (d != 0) {
				return d;
			}
			d = p.line.yIntercept - q.line.yIntercept;
			if (d != 0) {
				return d;
			}
		}
		return 0;
	}
	
	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		Function other = (Function) obj;
		return compareTo(other) == 0;
	}

	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder();
		sb.append(pieces);
		return sb.toString();
	}
}
