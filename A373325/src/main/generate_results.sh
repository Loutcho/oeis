#! /bin/bash

function f
{
	typeset -i depth=${1}
	typeset -r prefix="${2}"
	typeset -i i
	typeset -i n
	typeset new_prefix
	n=$( ./ncurve.exe ${prefix} )
	echo "${prefix} ==> ${n}"

	if [ $depth -eq 0 ]
	then
		return
	fi

	(( depth-=1 ))

	i=1
	while [ $i -le $n ]
	do
		if [ "${prefix}" == "" ]
		then
			new_prefix="${i}"
		else
			new_prefix="${prefix} ${i}"
		fi
		f ${depth} "${new_prefix}"

		(( i+=1 ))
	done
}

f 7 "" | tee results
