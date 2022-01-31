BEGIN {
	print "digraph G_" n "_" k
	print "{"
	print "node[shape = \"circle\" style=\"filled\" fillcolor=\"beige\"];"
	for (i=0; i<n; i++)
	{
		print i " -> " (k*i)%n ";"

	}
	print "}"
}
