#! /bin/bash
out=go_20000.txt
count=20000

rm $out
for ((i=0; i < count; i++)); 
  do 
  read line;
  echo $line >> $out
done 
