#! /bin/bash
out=bad_go_20000.txt
count=20000
pcterr=10
nterms=0

rm $out
for ((i=0; i < count; i++)); 
  do 
  read line
  if [ "$line" = "[Term]" ]; then
    if (( nterms % $pcterr == 0 )); then
	echo "[Form]" >> $out
    else
	echo $line >> $out
    fi
    let nterms++
  else
      echo $line >> $out
  fi
done
