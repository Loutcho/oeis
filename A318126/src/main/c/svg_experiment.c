#include <stdio.h>
#include <string.h>
#include <math.h>

#define C 50.0
#define N 250

#define M 27

static void a318126(double X, double Y, double *x, double *y)
{
	*y = 1 + pow(X, 1.5);
	*x = 1 + Y + X / 2.0;
}

static void fonc(double X, double Y, double *x, double *y)
{
	double n;
	double xx;
	double yy;
	double theta;
	double rho;
	n = (Y / X) + X;
	if (X >= n / 2.0) {
		xx = (Y / X) + X;
		yy = (Y / X);
	} else {
		//xx = X + sqrt(Y);
		//yy = sqrt(Y);
		xx = X + sqrt(Y);
		yy = sqrt(Y);
		/*theta = atan2(yy, xx);
		rho = sqrt(xx * xx + yy * yy);
		rho = pow(rho, 2.0 * yy / xx) / (2.0 * pow(yy / xx, 2.0));
		xx = rho * cos(theta);
		yy = rho * sin(theta);*/
	}

	*x = xx;
	*y = yy;
}

int main(int argc, char *argv[])
{
    FILE *f;
    int n;
    int k;
    double X, Y;
    double x, y;

    f = fopen("C:\\Users\\Luc\\Desktop\\x.svg", "w+");
    fprintf(f, "<?xml version=\"1.0\" standalone=\"no\"?>\n");
    fprintf(f, "<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\" \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">\n");
    X = N + 6.0;
    Y = 1.5 * N;
    fonc(X, Y, &x, &y);
    fprintf(f, "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"%f\" height=\"%f\">\n", 16180.0, 10000.0);
    for (n = 0; n <= N; n ++)
    {
    	X = 0;
    	Y = n + 0.5;
    	fonc(X, Y, &x, &y);
        fprintf(f, "<polyline points=\"");
        for (k = 1; k <= 3 * N / 2; k ++)
        {
        	X = k;
        	Y = (n - (n % k));
        	fonc(X, Y, &x, &y);
            fprintf(f, "%f,%f ", C * x, C * y);
        }

        switch (n)
        {
        	/*
		case M:
        	fprintf(f, "\" style=\"fill:none;stroke:red;stroke-width:5\" />\n");
        	break;
        	*/
		default:
			fprintf(f, "\" style=\"fill:none;stroke:black;stroke-width:1\" />\n");
			break;
        }
    }

/*
    for (n = 0; n <= N; n ++)
    {

        for (k = 1; k <= 3 * N / 2; k ++)
        {
        	X = k;
        	Y = (n - (n % k));
        	fonc(X, Y, &x, &y);
            fprintf(f, "<text x=\"%f\" y=\"%f\" >(%f,%f)</text>\n", C * x, C * y, n, k);
        }
	}
	*/

    fprintf(f, "</svg>\n");
    fclose(f);
}
