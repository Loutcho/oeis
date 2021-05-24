#include <stdio.h>
#include <limits.h>
#include <stdlib.h>
#include <string.h>

typedef long long int integer;
/*
 * => limitation to n <= 5
 */

#ifdef DEBUG
#define DEBUG_ONLY(x) { x; fflush(stdout); }
#else
#define DEBUG_ONLY(x)
#endif

#ifdef DEBUG
#define NONDEBUG_ONLY(x)
#else
#define NONDEBUG_ONLY(x) { x; fflush(stdout); }
#endif

integer neighbor(int v, int axis) {
	return v ^ (1 << axis);
}

int prune(int dim, int *mark, int w) {
	for (int axis = 0; axis < dim; axis ++) {
		int ww = neighbor(w, axis);
		if (mark[ww] == 1) {
			continue;
		}
		int nb_nonmarked_neighbors = 0;
		for (int axis2 = 0; axis2 < dim; axis2 ++) {
			int www = neighbor(ww, axis2);
			if (mark[www] == 0) {
				nb_nonmarked_neighbors ++;
			}
		}
		if (nb_nonmarked_neighbors <= 1) {
			return 1; // prune
		}
	}
	return 0; // do not prune
}

void explore(int dim, int max_axis, int *mark, integer v, int path_len, char *path, integer *count) {
	path_len ++;
	DEBUG_ONLY(
		char path_addition[20 + 1];
		sprintf(path_addition, "%d ", v);
		strcat(path, path_addition);
		printf("\n+++[%s]", path);
	)
	mark[v] = 1;

	if (path_len == (1 << dim)) {
		(*count) = (*count) + 1;
		DEBUG_ONLY(
			printf(" %c[36m%d%c[0m", 27, *count, 27);
		)
		NONDEBUG_ONLY(
			if (((*count) % 100000) == 0) {
				fprintf(stdout, "%lld\n", *count);
				fflush(stdout);
			}
		)
	} else {
		int virtual_new_max_axis = max_axis + (max_axis != dim - 1);
		DEBUG_ONLY(
			printf(" (%d)", virtual_new_max_axis + 1);
		)
		for (int axis = 0; axis <= virtual_new_max_axis; axis ++) {
			integer w = neighbor(v, axis);
			if (mark[w] == 0) {
				int p = prune(dim, mark, w);
				if ((!p) || (p && path_len == (1 << dim) - 2)) {
					DEBUG_ONLY(
						printf(" %c[32m>%c[0m", 27, 27);
					)
					int new_max_axis = (axis < virtual_new_max_axis) ? max_axis : virtual_new_max_axis;
					explore(dim, new_max_axis, mark, w, path_len, path, count);
				}
				else {
					DEBUG_ONLY(
						printf(" %c[31mX%c[0m", 27, 27);
					)
				}
			} else {
				DEBUG_ONLY(
					printf(" %c[33m.%c[0m", 27, 27);
				)
			}
		}
	}
	mark[v] = 0;
	if (strlen(path) > 0) {
		do {
			path[strlen(path) - 1] = '\0';
		} while ((strlen(path) > 0) && path[strlen(path) - 1] != ' ');
	}
	DEBUG_ONLY(
		printf("\n---[%s]", path);
	)
}

integer a(int dim) {
	integer count = 0;
	char path[500 + 1];
	int sz = (1 << dim) * sizeof(int);
	int *mark = (int *) malloc(sz);
	memset(mark, 0x0, sz);
	memset(path, 0x0, 100 + 1);
	explore(dim, -1, mark, 0, 0, path, &count);
	free(mark);
	return count;
}

int main(int argc, char *argv[]) {
	int n = atoi(argv[1]);
	fprintf(stdout, "a(%d) = %lld\n", n, a(n));
	fflush(stdout);
	/* A003043(n) = n! * a(n) */
}


