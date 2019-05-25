#include <stdio.h>
#include <string.h>
#include <stdlib.h>

int t(int k) { return k * (k + 1) / 2; }

int next(int n, int* x) {
	int i = 2; 	while ((i <= n) && (x[i] == 0)) { i++; }
	if (i > n) { return 0; }
	x[i] --; int r = x[1] + t(i); x[1] = 0;
	while (r > 0) { i--; int c = t(i); x[i] = r / c; r = r % c; }
	return 1;
}

int f(int n, int* x) {
	int s = 0; for (int i = 1; i <= n; i++) { s += x[i] * i; }
	return s;
}

int count(int n, int* nb) {
	int c = 0; int tt = t(n);
	for (int i = n; i <= tt; i++) { if (nb[i] != 0) { c++; } }
	return c;
}

int a(int n) {
	int szn = (n + 1) * sizeof(int); int* x = (int*)malloc(szn);
	memset(x, 0, szn);
	int szt = (t(n) + 1) * sizeof(int); int* nb = (int*)malloc(szt);
	memset(nb, 0, szt);
	x[n] = 1; do { nb[f(n, x)] ++; } while (next(n, x)); free(x);
	int c = count(n, nb); free(nb); return c;
}

int main() {
	int n = 1; while (1) { printf("%d, ", a(n++)); fflush(stdout); }
}
