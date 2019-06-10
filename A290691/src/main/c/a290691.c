#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#define NMAX 40
struct cell { int f; int v; };
struct line { struct cell t[NMAX]; };
void display(struct line *T) { int n, k; for (n = 3; n <= NMAX; n ++) { for (k = 1; k < n - 1; k ++) { printf("%2d, ", T[n].t[k].v); } printf("\n"); } }
void swap(int *a, int *b) { int x; x = *a; *a = *b; *b = x; }
void fill(struct line *T)
{
    int n, k;
    for (n = 3; n <= NMAX; n ++)
    {
        for (k = 1; k < n - 2; k ++)
        {
            T[n].t[k].v = T[n - 1].t[k].v - 1;
            T[n].t[k].f = T[n - 1].t[k].f;
        }
        T[n].t[n - 2].v = n - 2;
        T[n].t[n - 2].f = n - 1;
        for (k = 1; k < n - 2; k ++)
        {
            if ((T[n].t[k].v == 0) && (T[n].t[k + 1].v == 0))
            {
                swap(&T[n].t[k].f, &T[n].t[k + 1].f);
            }
        }
        for (k = 1; k < n - 2; k ++)
        {
            if (T[n].t[k].v == -1)
            {
                T[n].t[k].v += T[n].t[k].f;
            }
        }
    }
}
int main() { struct line T[NMAX + 1]; memset(T, 0x0, sizeof(T)); fill(T); display(T); }
