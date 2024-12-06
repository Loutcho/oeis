# Attempt to loop the generations outside from Prolog
# with persistence to intermediary files
# to try to avoid out-of-memory errors encountered for n >= 8.

function evolve()
{
	typeset -i M=${1}
	typeset -i N=${M}

	(( N += 1 ))

	GOAL="from_file_to_file(${M} / toto${M}s, ${N} / toto${N})."
	swipl.exe -f a378734_v1.pl -q -g "${GOAL}" -t halt

	sort toto${N} | uniq > toto${N}s
}

evolve $1
