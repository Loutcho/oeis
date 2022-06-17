#include <stdio.h>
#include <string.h>
#include <math.h>

#define C 8.0
#define N 500

static double my_sqrt(double x) {
	double xx = x;
	while (xx * xx - x > 0.00001)
	{
		xx = (xx + x / xx) / 2.0;
	}
	return xx;
}

static void fonc(double X, double Y, double *x, double *y)
{
	*x = 10.0 * my_sqrt(X);
	*y = 5.0 * my_sqrt(Y);
}

int main(int argc, char *argv[])
{
    FILE *f;
    int n;
    int k;
    double X, Y;
    double x, y;

    f = fopen("x.svg", "w+");
    fprintf(f, "<?xml version=\"1.0\" standalone=\"no\"?>\n");
    fprintf(f, "<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\" \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">\n");
    fprintf(f, "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"%f\" height=\"%f\">\n", 1800.0, 900.0);
    for (n = 0; n <= N; n ++)
    {
    	X = 0;
    	Y = C * n;
    	fonc(X, Y, &x, &y);
		/* fprintf(f, "<text font-size=\"%f\" x=\"%f\" y=\"%f\">%d</text>\n", C, x, y + C / 2.0, n); */
        fprintf(f, "<polyline points=\"");
        for (k = 1; k <= 3 * N / 2; k ++)
        {
        	X = k;
        	Y = (n - (n % k));
        	fonc(X, Y, &x, &y);
            fprintf(f, "%f,%f ", C * x, C * y);
        }

		fprintf(f, "\" style=\"fill:none;stroke:black;stroke-width:0.5\" />\n");
    }
    fprintf(f, "</svg>\n");
    fclose(f);
}
