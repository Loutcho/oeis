#include <stdio.h>
#include <limits.h>
#include <stdlib.h>
#include <string.h>

typedef long long int integer;

void bin(int n, char *b) {
	int o = 0;
	while (n > 0) {
		b[o ++] = '0' + (n & 1);
		n >>= 1;
	}
	b[o] = '\0';
}

integer neighbor(int v, int axis) {
	return v ^ (1 << axis);
}

void explore(int dim, int max_axis, int *mark, integer v, int path_len, integer *count)
{
	int axis;
	int virtual_new_max_axis;
	integer w;
	char b[10 + 1];
	/*
	printf("\n");
	for (int i = 0; i < path_len; i ++)
	{
		printf("\t");
	}
	*/
	path_len ++;
	mark[v] = 1;
	/*
	bin(v, b);
	fprintf(stdout, "(v=%s, path_len=%d, ", b, path_len);
	fflush(stdout);
	*/

	if (path_len == (1 << dim))
	{
		/*
		fprintf(stdout, " ##");
		fflush(stdout);
		*/
		(*count) = (*count) + 1;
		if (((*count) % 1000) == 0)
		{
			fprintf(stdout, "%d\n", *count);
			fflush(stdout);
		}
	}
	else
	{
		virtual_new_max_axis = max_axis + (max_axis != dim - 1);
		/*
		fprintf(stdout, "virtual_new_max_axis=%d", virtual_new_max_axis);
		fflush(stdout);
		*/
		for (axis = 0; axis <= virtual_new_max_axis; axis ++)
		{
			/*
			fprintf(stdout, "(axis=%d)", axis);
			fflush(stdout);
			*/
			w = neighbor(v, axis);
			if (mark[w] == 0)
			{
				int new_max_axis = (axis < virtual_new_max_axis) ? max_axis : virtual_new_max_axis;
				explore(dim, new_max_axis, mark, w, path_len, count);
			}
		}
	}
	mark[v] = 0;
	/*
	printf(")");
	fflush(stdout);
	*/
}

integer a(int dim)
{
	integer count = 0;
	int v = 0;
	int sz = (1 << dim) * sizeof(int);
	int *mark = (int *) malloc(sz);
	memset(mark, 0x0, sz);
	explore(dim, -1, mark, v, 0, &count);
	free(mark);
	return count;
}

int main(int argc, char *argv[])
{
	int n = atoi(argv[1]);

	integer an = a(n);
	fprintf(stdout, "a(%d) = %ld\n", n, an);
	fflush(stdout);
}


