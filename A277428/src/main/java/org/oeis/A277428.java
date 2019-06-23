package org.oeis;

public class A277428 {

	// Brute force implementation... Can it be improved?
	public static int PRIME[] = { 2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, /* to be continued */ };

	public static void main(String args[]) {
		int nMax = PRIME.length; // number of terms of the sequence
		for (int n = 1; n <= nMax; n++) {
			if (n > 1) {
				System.out.print(", ");
			}
			System.out.print(u(n));
		}
	}

	private static int u(int n) {
		double bestMul = 0.0;
		int bestSetup = -1;
		int s = 0; // binary-encoded setup number
		for (s = 0; s < (1 << n); s++) {
			double mul = 1.0;
			int i = 0; // prime number #
			for (i = 0; i < n; i++) {
				if ((s & (1 << i)) != 0) {
					mul *= PRIME[i]; // 1 = above fraction bar
				} else {
					mul /= PRIME[i]; // 0 = below fraction bar
				}
			}
			if (mul < 1.0) {
				if (mul > bestMul) {
					bestMul = mul;
					bestSetup = s;
				}
			}
		}
		return bestSetup;
	}
}
