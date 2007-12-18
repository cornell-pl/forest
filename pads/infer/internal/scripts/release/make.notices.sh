#!/bin/sh


function insertCR {
for p in `cat $1`
do
  echo $p:
  for f in `find $p -type f`
  do
	echo Updating $f
	proto -l  internal/scripts/infer.lic -hpr -c"$2" -o author=$3 $f
  done
done
}

echo inserting notices
pwd
insertCR scripts/release/sml.files  '(*)' kfisher+kzhu+white


    












