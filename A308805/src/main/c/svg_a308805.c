#include <stdio.h>
#include <math.h>

#define WIDTH 1800
#define HEIGHT 900

#define X_MAX 200
#define Y_MAX 100

#define M_MAX 6
#define N_MAX 11

#define D_MAX 16

void c(double x, double y, int *xx, int *yy)
{
	*xx = (int) rint(40.0 + 80.0 * x);
	*yy = HEIGHT - (int) rint(40.0 + 80.0 * y);
}

void grid(FILE *f)
{
	double x1, y1, x2, y2;
	int xx1, yy1, xx2, yy2;
	for (int x = 0; x <= X_MAX; x ++)
	{
		x1 = x;
		y1 = 0.0;
		x2 = x;
		y2 = Y_MAX;
		c(x1, y1, &xx1, &yy1);
		c(x2, y2, &xx2, &yy2);
		fprintf(f, "<line x1=\"%d\" y1=\"%d\" x2=\"%d\" y2=\"%d\" style=\"stroke:rgb(0,0,0);stroke-width:1\"/>\n", xx1, yy1, xx2, yy2);
	}

	for (int y = 0; y <= Y_MAX; y ++)
	{
		x1 = 0.0;
		y1 = y;
		x2 = X_MAX;
		y2 = y;
		c(x1, y1, &xx1, &yy1);
		c(x2, y2, &xx2, &yy2);
		fprintf(f, "<line x1=\"%d\" y1=\"%d\" x2=\"%d\" y2=\"%d\" style=\"stroke:rgb(0,0,0);stroke-width:1\"/>\n", xx1, yy1, xx2, yy2);
	}
}

void labels(FILE *f)
{
	for (int x = 0; x <= X_MAX; x ++)
	{
		for (int y = 0; y <= Y_MAX; y ++)
		{
			int xx, yy;
			int p = ((x + y) * (x + y) + 3 * x + y) / 2;
			c(x, y, &xx, &yy);
			fprintf(f, "<text x=\"%d\" y=\"%d\" fill=\"gray\">%d</text>\n", xx, yy, p);
		}
	}
}

void red_m_lines(FILE *f)
{
	for (int m = 1; m <= M_MAX; m ++)
	{
		double x1, y1, x2, y2;
		int xx1, yy1, xx2, yy2;
		x1 = 0.0;
		y1 = 2.0 * (m - 1);
		x2 = X_MAX;
		y2 = y1 + ((double) (m - 1)) / ((double) (m + 1)) * X_MAX;
		c(x1, y1, &xx1, &yy1);
		c(x2, y2, &xx2, &yy2);
		fprintf(f, "<line x1=\"%d\" y1=\"%d\" x2=\"%d\" y2=\"%d\" style=\"stroke:rgb(255,0,0);stroke-width:3\"/>\n", xx1, yy1, xx2, yy2);
	}
}

void blue_n_lines(FILE *f)
{
	for (int n = 1; n <= N_MAX; n ++)
	{
		double x1, y1, x2, y2;
		int xx1, yy1, xx2, yy2;
		x1 = 2.0 * (n - 1);
		y1 = 0.0;
		x2 = x1 + ((double) (n - 1)) / ((double) (n + 1)) * Y_MAX;
		y2 = Y_MAX;
		c(x1, y1, &xx1, &yy1);
		c(x2, y2, &xx2, &yy2);
		fprintf(f, "<line x1=\"%d\" y1=\"%d\" x2=\"%d\" y2=\"%d\" style=\"stroke:rgb(0,0,255);stroke-width:3\"/>\n", xx1, yy1, xx2, yy2);
	}
}

void green_antidiagonals(FILE *f)
{
	for (int d = 0; d <= D_MAX; d ++)
	{
		double x1, y1, x2, y2;
		int xx1, yy1, xx2, yy2;
		x1 = -0.5;
		y1 = 2.0 * d + 0.5;
		x2 = 2.0 * d + 0.5;
		y2 = -0.5;
		c(x1, y1, &xx1, &yy1);
		c(x2, y2, &xx2, &yy2);
		fprintf(f, "<line x1=\"%d\" y1=\"%d\" x2=\"%d\" y2=\"%d\" style=\"stroke:rgb(0,255,0);stroke-width:2\"/>\n", xx1, yy1, xx2, yy2);
	}
}

void circled_labels(FILE *f)
{
	for (int d = 0; d <= D_MAX; d ++)
	{
		for (int m = 1; m <= d + 1; m ++)
		{
			if (((d + 1) % m) == 0)
			{
				int n = (d + 1) / m;
				int xx, yy;
				int x = (m + 1) * (n - 1);
				int y = (m - 1) * (n + 1);
				int p = ((x + y) * (x + y) + 3 * x + y) / 2;
				c(x, y, &xx, &yy);
				fprintf(f, "<circle cx=\"%d\" cy=\"%d\" r=\"%d\" style=\"fill:green;stroke:rgb(0,255,0);stroke-width:2;fill-opacity:0.5\"/>\n", xx, yy, 20);
				fprintf(f, "<text x=\"%d\" y=\"%d\" font-weight=\"bold\" fill=\"black\">%d</text>\n", xx, yy, p);
			}
		}
	}
}

int main(int argc, char*argv[])
{
	int m, n, x, y, d;
	FILE *f = fopen("C:\\Users\\Luc\\Desktop\\truc.html", "w+");

	if (f == NULL)
	{
		fprintf(stderr, "Erreur\n");
	}

	fprintf(f, "<html>\n");
	fprintf(f, "<body>\n");
	fprintf(f, "<svg width=\"%d\" height=\"1000\">\n", WIDTH, HEIGHT);
			
	grid(f);
	green_antidiagonals(f);
	labels(f);
	red_m_lines(f);
	blue_n_lines(f);
	circled_labels(f);

	fprintf(f, "</svg>\n");
	fprintf(f, "</body>\n");
	fprintf(f, "</html>\n");

	fclose(f);
}
