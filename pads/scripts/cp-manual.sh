#!/bin/sh

echo creating tarball for manual
cd $PADS_HOME/documents/manual
make
mkdir temp_manual
mkdir temp_manual/pictures
mkdir temp_manual/code
mkdir temp_manual/hand_code
for x in `cat take_list`; do cp -r $x temp_manual/$x; done
tar cvf manual.tar temp_manual 
gzip manual.tar
rm -rf temp_manual

echo moving tarball to www.padsproj.org
scp manual.tar.gz www.padsproj.org:~kfisher/manual.tar.gz

rm manual.tar.gz
