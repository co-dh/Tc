#!/bin/bash
export LD_LIBRARY_PATH=/usr/local/lib

for f in data/*.txt; do
  result=$(timeout 2 script -q -c "stty rows 24 cols 80; .lake/build/bin/tc '$f' -c ''" /dev/null 2>&1 | ansi2txt | tail -1)
  cols=$(echo "$result" | grep -oP 'c\d+/\K\d+' || echo "?")
  rows=$(echo "$result" | grep -oP 'r\d+/\K\d+' || echo "?")
  name=$(basename "$f")
  printf "%-25s r=%-4s c=%s\n" "$name" "$rows" "$cols"
done
