package org.oeis;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.oeis.Direction;

public class DirectionTest {

	@Test
	public void test() {
		
		// implicitly a direction is a mere angle in [0, 2*Pi)
		
		Direction[] d = new Direction[8];
		d[0] = new Direction(+1,  0);
		d[1] = new Direction(+1, +1);
		d[2] = new Direction( 0, +1);
		d[3] = new Direction(-1, +1);
		d[4] = new Direction(-1,  0);
		d[5] = new Direction(-1, -1);
		d[6] = new Direction( 0, -1);
		d[7] = new Direction(+1, -1);
		
		Direction[] dd = new Direction[8];
		dd[0] = new Direction(+2,  0);
		dd[1] = new Direction(+2, +1);
		dd[2] = new Direction( 0, +2);
		dd[3] = new Direction(-2, +1);
		dd[4] = new Direction(-2,  0);
		dd[5] = new Direction(-2, -1);
		dd[6] = new Direction( 0, -2);
		dd[7] = new Direction(+2, -1);

		Assertions.assertTrue(d[0].compareTo(dd[0]) == 0); // (+1,  0) == (+2,  0)
		Assertions.assertTrue(d[1].compareTo(dd[1])  > 0); // (+1, +1)  > (+2, +1)
		Assertions.assertTrue(d[2].compareTo(dd[2]) == 0); // ( 0, +1) == ( 0, +2)
		Assertions.assertTrue(d[3].compareTo(dd[3])  < 0); // (-1, +1)  < (-2, +1)
		Assertions.assertTrue(d[4].compareTo(dd[4]) == 0); // (-1,  0) == (-2,  0)
		Assertions.assertTrue(d[5].compareTo(dd[5])  > 0); // (-1, -1)  > (-2, -1)
		Assertions.assertTrue(d[6].compareTo(dd[6]) == 0); // ( 0, -1) == ( 0, -2)
		Assertions.assertTrue(d[7].compareTo(dd[7])  < 0); // (+1, -1)  < (+2, -1)
		
		Assertions.assertTrue(d[0].compareTo(dd[7])  < 0); // (+1,  0)  < (+2, -1)
	}
}
