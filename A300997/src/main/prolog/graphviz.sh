GV="moitmoit.gv"

> $GV
echo "digraph G {" >> $GV
echo "	rankdir=\"LR\";" >> $GV
echo "	node [shape=\"none\" style=\"filled\" fillcolor=\"#BBBBBB\"];" >> $GV
echo "	edge [arrowhead=\"none\" arrowtail=\"normal\" dir=\"back\"];" >> $GV
n=1
while [ $n -le 16 ]
do
	echo "\"[$n]\" [label=\"[$n]\" fillcolor=\"#BBFFBB\"];" >> $GV
	(( n+=1 ))
done
cat graphviz_lines.gv | awk '
# remove duplicates while keeping order of first appearance
{
	if (!($0 in mem))
	{
		print $0;
		mem[$0] = 1;
	}
}
' >> $GV
echo "}" >> $GV

./dot_svg.bat $GV

