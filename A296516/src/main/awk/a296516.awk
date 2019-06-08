function eat(R, d, i, j)
{
	if (!(d in R) || (i < R[d][lo])) R[d][lo] = i;
	if (!(d in R) || (R[d][hi] < j)) R[d][hi] = j;
}

function min(x, y) { if (0 + x < 0 + y) { return x } else { return y } }
function max(x, y) { if (0 + x < 0 + y) { return y } else { return x } }

function count(R)
{
	sum = 0;
	for (d in R)
	{
		x = R[d][hi] - R[d][lo] + 1;
		if (d == 0) { sum += x } else { sum += 2 * x }
#		printf("(%d, %d-%d)", d, R[d][lo], R[d][hi]);
	}
	return sum;
}

BEGIN {
	lo = 0;
	hi = 1;
	P[1][1][lo] = 0;
	P[1][1][hi] = 0;
	Q[1][0][lo] = 1;
	Q[1][0][hi] = 1;
	printf("%d, ", count(Q[1]));
	for (n = 2; n <= 20; n ++)
	{
		P[n][-1] = 0; delete P[n][-1];
		Q[n][-1] = 0; delete Q[n][-1];
		for (d in P[n - 1]) eat(P[n], d, P[n - 1][d][lo], P[n - 1][d][hi]);
		for (e in Q[n - 1]) eat(P[n], e, Q[n - 1][e][lo], Q[n - 1][e][hi]);
		for (d in P[n - 1])
		{
			for (e in Q[n - 1])
			{
				o = min(d, e);
				O = max(d, e);
				ee = d + e;
				eat(Q[n], ee, P[n - 1][d][lo] + Q[n - 1][e][lo], P[n - 1][d][hi] + Q[n - 1][e][hi]);
				ee = O - o;
#		printf("[d=%d, e=%d, o=%d, O=%d, ee=%d]", d, e, o, O, ee);
				eat(Q[n], ee, P[n - 1][d][lo] + Q[n - 1][e][lo] + o, P[n - 1][d][hi] + Q[n - 1][e][hi] + o);
			}
		}
		printf("%d, \n", count(Q[n]));
	}
	printf("\n");
}
{
}
