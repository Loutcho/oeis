package org.oeis;

public class Piece {

	public Interval intervalOfDefinition;
	public Line line;
	
	public Piece(Interval i, Line l) {
		this.intervalOfDefinition = i;
		this.line = l;
	}
	
	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder();
		sb.append(intervalOfDefinition);
		sb.append(":");
		sb.append(line);
		return sb.toString();
	}
}
