#! /bin/bash

# Manages the iterations outside from Prolog with persistence to intermediary files,
# to avoid out-of-memory errors encountered from n = 8 when all is in Prolog.
# Can reach n = 11 but n = 12 is still too much.

# File names:
# - x.nn ==> raw output from Prolog (with duplicates, not sorted)
# - y.nn ==> with duplicates, sorted
# - z.nn ==> clean version: no duplicates, sorted

function log()
{
	DATE=$(date "+%Y/%m/%d %H:%M:%S");
	echo "${DATE} $*"
}

function evolve()
{
	typeset -i M=${1}

	# N = M + 1
	typeset -i N=${M}
	(( N += 1 ))

	# formatting on 2 digits
	MM=$(printf "%02d" ${M})
	NN=$(printf "%02d" ${N})

	log "Calling Prolog"
	GOAL="from_file_to_file(${M} / 'z.${MM}', ${N} / 'x.${NN}')."
	swipl.exe -f a378734_v1.pl -q -g "${GOAL}" -t halt
	q=$?
	log "Prolog returned with exit code: $q"
	if [ $q -ne 0 ]
	then
		return 1
	fi

	log "Calling sort"
	sort x.${NN}  > y.${NN}
	q=$?
	log "sort returned with exit code: $q"
	if [ $q -ne 0 ]
	then
		return 2
	fi

	log "Calling uniq"
	uniq y.${NN} z.${NN}
	q=$?
	log "uniq returned with exit code: $q"
	if [ $q -ne 0 ]
	then
		return 3
	fi

	AN=$(cat z.${NN} | wc -l)
	log "a($N) = $AN"
}

N=0
q=0
while [ $q -eq 0 ]
do
	echo "---------------- N=${N} -------------------"

	log "Begin"

	evolve ${N}
	q=$?

	log "End"

	(( N += 1 ))
done

exit $q
