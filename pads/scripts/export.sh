#!/bin/sh

cvs -d :ext:cvs-graphviz.research.att.com:/cvsroot export -D today pads

# build documentation
cd pads/documents/manual
make manual-all
make manual_html

# install documentation
mkdir ../../temp_docs
for x in `cat take_list`; do cp $x ../../temp_docs; done
cd ../..
rm -rf documents
mv temp_docs documents

# remove internal directories/files
rm Changes
rm -rf GIGASCOPE_README
rm -rf Notes

#remove .cvs files
for x in `find . -name .cvsignore`; rm $x ; done

# clean example directory
cd padsc/examples
#clean p directory
mkdir temp_p
for x in `cat p/RELEASE_PFILES`; do cp p/$x temp_p; done
rm -rf p
mv temp_p p

#clean test directory
mkdir temp_tests
for x in `cat p/RELEASE_TESTS`; do cp p/$x temp_tests; done
rm -rf tests
mv temp_tests tests

#clean data directory
mkdir temp_data
for x in `cat data/take_list`; do cp data/$x temp_data; done
rm -rf data 
mv temp_data data

cd .. # now in padsc directory
mkdir temp_example
for x in `cat example/take_list`; do mv example/$x temp_example; done
rm -rf example
mv temp_example example

# add licenses!

#okay, tar up the desired files
tar cfz pads.tar.gz `cat pads/take_list`
