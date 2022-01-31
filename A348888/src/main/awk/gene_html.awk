BEGIN {
	print "<table border='1' cellspacing='0'>"
	row0();
	for (n=N_MIN; n<=N_MAX; n++)
	{
		row(n);
	}
	print "</table>"
}

function row0() {
	print "\t<tr>"
	print "<th>&nbsp;</th>"
	for (k=0; k<=N_MAX-1; k ++)
	{
		print "<th>k&nbsp;=&nbsp;" k "</th>"
	}
	print "\t</tr>"
}

function row(n) {
	print "\t<tr>"
	cell0(n);
	for (k=0; k<=n-1; k ++)
	{
		cell(n,k)
	}
	print "\t</tr>"
}

function cell0(n) {
	print "\t\t<th align='center'>n&nbsp;=&nbsp;" n "</th>"
}

function cell(n,k) {
	system("awk -f cell.awk -v n=" n " -v k=" k " > g_" n "_" k ".dot");
	system("./graphviz.cmd g_" n "_" k ".dot")
	print "\t\t<td align='center'><img src='g_" n "_" k ".dot.svg' /></td>"
}

