function polynomial_to_array_of_coefficients(P, c, monomials, monomial_index, monomial, equality, position, coefficient) {
	split(P, monomials, ",");
	for (monomial_index = 1; monomial_index <= length(monomials); monomial_index ++) {
		monomial = monomials[monomial_index];
		split(monomial, equality, "=");
		position = equality[1];
		coefficient = equality[2];
		c[position] = coefficient;
	}
}

function array_of_coefficients_to_polynomial(c, n, P) {
	P = "";
	n = 0;
	for (position in c) {
		if (n > 0) { P = P ","; }
		P = P position "=" c[position];
		n ++;
	}
	return P;
}

function clone(c, cc) {
	for (position in c) { cc[position] = c[position]; }
}

function successors(P, S, c, position, cc, coordinates, i, j, ii, jj, PP) {
	split("", c, ":");
	polynomial_to_array_of_coefficients(P, c);
	for (position in c) {
		split("", cc, ":");
		clone(c, cc);
		split(position, coordinates, "-");
		i = coordinates[1];
		j = coordinates[2];
		ii = i + 1;
		jj = j + 1;
		cc[ii "-" j] ++;
		cc[i "-" jj] ++;
		cc[i "-" j] --;
		if (cc[i "-" j] == 0) { delete cc[i "-" j]; }
		PP = array_of_coefficients_to_polynomial(cc);
		S[PP] = 1;
	}
}

function predecessors(P, S, position, cc, coordinates, i, j, ii, jj, PP) {
	split("", c, ":");
	polynomial_to_array_of_coefficients(P, c);
	for (position in c) {
		split("", cc, ":");
		clone(c, cc);
		split(position, coordinates, "-");
		ii = coordinates[1];
		j = coordinates[2];
		i = ii - 1;
		jj = j + 1;
			print "/* --- " i " --- " j " --- " cc[ii "-" j] " --- " cc[i "-" jj] " --- */";
		if (i >= 0 && j >= 0 && cc[ii "-" j] > 0 && cc[i "-" jj] > 0) {
			cc[ii "-" j] --;
			if (cc[ii "-" j] == 0) { delete cc[ii "-" j]; }
			cc[i "-" jj] --;
			if (cc[i "-" jj] == 0) { delete cc[i "-" jj]; }
			cc[i "-" j] ++;
			PP = array_of_coefficients_to_polynomial(cc);
			S[PP] = 1;
		}
	}
}

function new_successors(P, S, SS, PP) {
	successors(P, SS);
	clone(SS, S);
	for (PP in SS) {
		if (is_in_gray(PP) || is_in_black(PP)) {
			delete S[PP];
		}
	}
}

function new_predecessors(P, S, SS, PP) {
	predecessors(P, SS);
	clone(SS, S);
	for (PP in SS) {
		if (is_in_gray(PP) || is_in_black(PP)) {
			delete S[PP];
		}
	}
}

function add_to_gray(polynomial, priority) {
	gray1[polynomial] = 1; # exists
	gray2[priority] = polynomial; # priority management
}

function is_in_gray(polynomial) {
	return (polynomial in gray1);
}
function is_in_black(polynomial) {
	return (polynomial in black);
}

function choose_gray(m) {
	m = min_key(gray2);
	P = gray2[m];
	delete gray1[P];
	delete gray2[m];
	black[P] = 1; # exists
	return P;
}

function polynomial_to_string(P) {
	return "polynomial(" P ")";
}

function keys_to_string(T, s, n) {
	s = "[";
	n = 0;
	for (k in T) {
		if (n > 0) { s = s ", "; }
		s = s k;
		n ++;
	}
	s = s "]#" length(T);
	return s;
}

function values_to_string(T, s, n) {
	s = "[";
	n = 0;
	for (k in T) {
		if (n > 0) { s = s ", "; }
		s = s T[k];
		n ++;
	}
	s = s "]#" length(T);
	return s;
}

function map_to_string(T, s, n) {
	s = "[";
	n = 0;
	for (k in T) {
		if (n > 0) { s = s ", "; }
		s = s k ":" T[k];
		n ++;
	}
	s = s "]#" length(T);
	return s;
}

function min_key(T, m, n) {
	m = 0;
	n = 0;
	for (k in T) {
		if (n == 0) {
			m = (0 + k);
		} else {
			if (0 + k < 0 + m) {
				m = (0 + k);
			}
		}
		n ++;
	}
	return m;
}

function max_key(T, m, n) {
	m = 0;
	n = 0;
	for (k in T) {
		if (n == 0) {
			m = (0 + k);
		} else {
			if (0 + k > 0 + m) {
				m = (0 + k);
			}
		}
		n ++;
	}
	return m;
}

function print_state() {
	print "gray1: " keys_to_string(gray1);
	print "gray2: " map_to_string(gray2);
	print "black: " keys_to_string(black);
}

function print_graphviz_header() {
	print "digraph G {";
	#print "\trankdir = \"LR\";";
	#print "\tnode[shape = \"box\" style = \"filled\" fillcolor = \"beige\"];";
	print "\tnode[shape = \"point\"];";
	print "\tedge[arrowhead = \"none\" penwidth = \"4\"];";
}

function print_graphviz_footer() {
	print "}";
}

BEGIN {
	PROCINFO["sorted_in"] = "@ind_str_asc";
	print_graphviz_header();
	if (N == 0) { N = 5; }
	split("", gray1, ",");
	split("", gray2, ",");
	split("", black, ",");
	P0 = "0-0=1";
	add_to_gray(P0, 0);
	n = 0;
	while (n <= N) {
		#print "=============== Iteration " n "===================";
		# print_state();
		P = choose_gray();
		#print "Chosen P: " P;

		split("", s, ",");
		new_predecessors(P, s);
		split("", S, ",");
		new_successors(P, S);
		#print "Successors of P that are new: " keys_to_string(S);
	
		for (PP in s) {
			m = min_key(gray2);
			add_to_gray(PP, m - 1);
			print "\t\"" PP "\" -> \"" P "\"[color = \"red\"];";
		}
		for (PP in S) {
			m = max_key(gray2);
			add_to_gray(PP, m + 1);
			print "\t\"" P "\" -> \"" PP "\";";
		}
		n ++;
	}
	print_graphviz_footer();
}
