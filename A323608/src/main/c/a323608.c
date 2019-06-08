#include <stdio.h>

int a(int n) {
	int q = n / 4;
	int r = n % 4;
	if (q == 0) return 1;
	switch (r) {
	case 0: return 3*q - 2 + a(q);
	case 1: return 2*q;
	case 2: return 3*q;
	case 3: return 2*q + 1;
	}
}

int main(int argc, char *argv[]) {
	int n;
	int N = (argc == 1) ? 50 : atoi(argv[1]);
	for(n = 1; n <= N; n ++) {
		printf("%d, ", a(n));
	}
	printf("\n");
}
