FILE="${1}"

AN=$(cat "${FILE}" | sort | uniq -c | grep -c ^)
echo "==> ${AN}"
