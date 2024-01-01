package org.oeis;

import java.util.Set;
import java.util.TreeSet;

/*
 * This program explores a graph of polynomials that are bivariate (x, y) and with nonnegative integers.
 * The edges are directed and of two types:
 * - edge type 1 "degradation": P1 -> P2 if P2 can be obtained by degrading P1
 *   (i.e. there exists some (a, b) such that the coefficient of x^a*y^b can be decremented
 *    while these of x^(a+1)*y^b and x^a*y^(b+1) can be incremented 
 * - edge type 2 "elevation": P1 -> P2 if P2 can be obtained by elevating P1
 *   (i.e. there exists some (a, b) such that the coefficient of x^a*y^b can be incremented
 *    while these of x^(a+1)*y^b and x^a*y^(b+1) can be decremented
 * The starting vertex is the polynomial: 1.
 * 
 * The purposes of all this are:
 * - to detect Gardens of Eden (polynomials that cannot be elevated) other than 1  
 * - to get insights about the minimal required depth of exploration in order to get all the polynomials that account for A363933(n) for a given n.
 *   For example, x^3 + 3xy + y^3 accounts for A363933(5) but an exploration at depth 7 is required to reach it.
 * - to prove that polynomials exist such that one needs actually two pairs of (degrade, elevate), not just one, to reach them
 *   This is now proved,
 *             P = x^4 + x^4y + x^2y^2 + 3xy + xy^4 + y^4
 *             has P(1,1) = 8 yet requires a depth 12 to be reached.
 * - to try to confirm the plausible/intuitive fact that there are no "isolated" polynomials
 *   (polynomials that would account for A363933 but that would be unreachable by this method)
 */
public class A {

	public static void main(String[] args) { new A().explore(); }
	private A() {}
	
	private static final int DISTANCE_MAX = 18;
	
	private void explore() {

		ExplorationData explorationData = new ExplorationData();
		
		Polynomial p0 = new Polynomial();
		p0.coef.put(new Coord(0, 0), 1);
		
		int d = 0;
		explorationData.add(p0, d);
		
		while (d < DISTANCE_MAX) {
			d ++;
			for (Polynomial v : explorationData.byDistance.get(d - 1)) {
				Set<Polynomial> degraded = Degrader.degrade(v);
				Set<Polynomial> elevated = Elevator.elevate(v);
				if (elevated.isEmpty()) {
					int s = v.sumOfCoefficients();
					if (d - s > 2) {
						System.out.println("s=" + s + ", d=" + d + ", v=" + v);
					}
				}
				Set<Polynomial> neighbours = new TreeSet<>();
				neighbours.addAll(degraded);
				neighbours.addAll(elevated);
				for (Polynomial neighbour : neighbours) {
					if (! explorationData.explored.contains(neighbour)) {
						explorationData.add(neighbour, d);
					}
				}
			}
			/*
			for (int i = 1; i <= d + 1; i ++) {
				System.out.println(String.format("d=%d, i=%d: %d", d, i, explorationData.byEvalAt1.get(i).size()));	
			}
			*/
		}
	}
}
