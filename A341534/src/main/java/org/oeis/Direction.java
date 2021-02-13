package org.oeis;

public class Direction implements Comparable<Direction> {

	public int x;
	public int y;
	
	public Direction(int x, int y) {
		assert(x != 0 || y != 0);
		this.x = x;
		this.y = y;
	}
	
	public static Direction intermediary(Direction a, Direction b) {
		return new Direction(a.x + b.x, a.y + b.y);
	}
	
	public Direction rotateCounterclockwise() {
		return new Direction(y, -x);
	}

	private int signum(int q) {
		if (q > 0) return +1;
		if (q < 0) return -1;
		return 0;
	}
	
	/*
	 *  3 | 2 | 1
	 * ---+---+---
	 *  4 | / | 0 
	 * ---+---+---
	 *  5 | 6 | 7
	 */
	private int zone() {
		int sx = signum(x);
		int sy = signum(y);
		switch (3 * sx + sy) {
		case -4 /* = 3 * (-1) + (-1) */: return 5;
		case -3 /* = 3 * (-1) + ( 0) */: return 4;
		case -2 /* = 3 * (-1) + (+1) */: return 3;
		case +1 /* = 3 * ( 0) + (+1) */: return 2;
		case -1 /* = 3 * ( 0) + (-1) */: return 6;
		case +2 /* = 3 * (+1) + (-1) */: return 7;
		case +3 /* = 3 * (+1) + ( 0) */: return 0;
		case +4 /* = 3 * (+1) + (+1) */: return 1;
		default:
			throw new IllegalStateException();
		}
	}
	
	@Override
	public int compareTo(Direction that) {
		int z = this.zone();
		int zz = that.zone();
		if (z != zz) {
			return z - zz;
		}
		return this.y * that.x - that.y * this.x;
	}

	@Override
	public String toString() {
		return "(" + x + ", " + y + ")";
	}
}
