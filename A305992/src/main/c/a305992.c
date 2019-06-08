#include <stdio.h>
#include <string.h>
#define N 10000
void e(int *t, int *s) {
	int T[N], i = 0; memset(T, 0, sizeof(T));
	while (i < *s) {
		int f = t[i] / 2;
		T[i] += f + (t[i] % 2);
		T[++ i] += f;
	}
	if (T[*s] != 0) { *s += 1; }
	for (i = 0; i < *s; i ++) { t[i] = T[i]; }
}
int f(int n) {
	int t[N], s = 1, i = 0; t[0] = n;
	while (s != n) { i ++; e(t, &s); }
	return 2 * n - i;
}
int main() {
	int n, last = 1, current;
	for (n = 1; n <= N; n ++) {
		current = f(n);
		switch (current - last) {
		case 0: break;
		case 1: printf("%d, ", n); fflush(stdout); break;
		default: fprintf(stderr, "CONJECTURE IS FALSE - last = %d, current = %d\n", last, current); return;
		}
		last = current;
	}
	printf("\n");
}
