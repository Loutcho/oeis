FILE=$1

cat $FILE | cut -f 3 -d '|' | sort | uniq -c | wc -l
