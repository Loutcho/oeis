#include <stdio.h>
#include <stdlib.h>
#include <string.h>
int main(int argc, char *argv[]) {
	if (argc != 2)
	{
		return 1;
	}
	int N = atoi(argv[1]);
	char *t = malloc(N + 1);
	int p, n = 2;
	memset(t, 0x0, sizeof(t));
x:
	printf("%d, ", n);
	fflush(stdout);
	p = n;
	while (p <= N) { t[p] = 1; p += n; }
	while (n <= N && t[n]) n ++;
	if (n <= N) goto x;
	printf("\n");
	free(t);
	return 0;
}
