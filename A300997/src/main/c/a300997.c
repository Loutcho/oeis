#include <stdio.h>
#include <string.h>
#define N 100
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
int a(int n) {
		int t[N], s = 1, i = 0; t[0] = n;
		while (s != n) { i ++; e(t, &s); }
		return i;
}
int main() { int n; for (n = 1; n <= N; n ++) { printf("%d, ", a(n)); } printf("\n"); }
