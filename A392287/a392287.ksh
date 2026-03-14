From=$1
To=$2

N=$From
while [ $N -le $To ]
do
	NN=$(printf %02d $N)
	Filename="toto${NN}"
	swipl -t halt -q -g 'main('${Filename}', '${N}').' -f a392287.pl
	# cat "${Filename}" | sort -n | uniq -c > "${Filename}.su"
	# Count=$(grep -c ^ "${Filename}.su")
	Count=$(cat "${Filename}" | sort -n | uniq -c | wc -l)
	printf "${Count}, "
	(( N += 1 ))
done

echo
