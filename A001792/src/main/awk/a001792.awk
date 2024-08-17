# Idea: how many words can we get with n replacements?
#
# - <> is the starting word;
# - it is legal to replace any < by [>]
# - it is legal to replace any < by <[]
# - it is legal to replace any > by []>
# - it is legal to replace any > by [<]
#
# Conjecturally, provides A001792

function count(array,  n) {
	n = 0;
	for (x in array) { n ++; }
	return n;
}

function add_children(s, new_population,  len, c, u, v) {
	len = length(s);
	for (i = 1; i <= len; i ++) {
		c = substr(s, i, 1);
		if (c == "<") {
			u = substr(s, 1, i - 1);
			v = substr(s, i + 1, len - i);
			new_population[u "[>]" v] ++;
			new_population[u "<[]" v] ++;
		}
		if (c == ">") {
			u = substr(s, 1, i - 1);
			v = substr(s, i + 1, len - i);
			new_population[u "[]>" v] ++;
			new_population[u "[<]" v] ++;
		}
	}
}

function evolve(old_population, new_population) {
	for (x in old_population) {
		add_children(x, new_population);
	}
}

function overwrite(src, dst) {
	for (x in dst) { delete dst[x]; }
	for (x in src) { dst[x] = src[x]; }
	for (x in src) { delete src[x]; }
}

BEGIN {
	iteration = 0;
	population["<>"] = 1;
	printf count(population) ", ";
	split("", new_population);
	while (iteration < 20) {
		iteration ++;
		evolve(population, new_population);
		overwrite(new_population, population);
		printf count(population) ", ";
	}
}
