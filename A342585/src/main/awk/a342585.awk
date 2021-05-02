function record(k) {
	printf("%d %d\n", i, k);
	number[k]++;
	i++;
	if (i > I_MAX) {
		exit;
	}
}

function inventory() {
	n = 0;
	while (number[n] != 0) {
		record(number[n]);
		n++;
	}
	record(0);
	inventory();
}

BEGIN {
	if (I_MAX == 0) { I_MAX = 25000 }
	i = 1;
	inventory();
}
