set -x

N=$1

GOAL="time(a($N))."

swipl -q -f a266549.pl -g "$GOAL" -t halt
echo "Return code: $?"
