#!/bin/sh

cvs -d :ext:cvs-graphviz.research.att.com:/cvsroot export -D today pads

# build documentation
cd pads/documents/manual
make manual-all
make manual_html

# install documentation
mkdir ../../temp_docs
for x in `cat take_list`; do cp $x ../../temp_docs; done
cd ../..  # now at pads
rm -rf documents
mv temp_docs documents


# remove internal directories/files
rm Changes
rm -rf GIGASCOPE_README
rm -rf Notes

#remove .cvs files
#rules.mk generates entries for some of these, so we're going to leave 
#the residual files in to avoid duplicating rules.mk
#for x in `find . -name .cvsignore`; do rm $x ; done

# clean example directory
cd padsc/examples
pwd
#clean p directory
echo cleaning p directory
mkdir temp_p
for x in `cat p/RELEASE_PFILES`; do cp p/$x temp_p; done
rm -rf p
mv temp_p p

#clean test directory
echo cleaning test directory
mkdir temp_tests
for x in `cat tests/RELEASE_TESTS`; do cp tests/$x temp_tests; done
rm -rf tests
mv temp_tests tests

#clean data directory
echo cleaning data directory
mkdir temp_data
for x in `cat data/take_list`; do cp data/$x temp_data; done
rm -rf data 
mv temp_data data

cd ..  # now in pads/padsc
echo cleaning example directory
pwd
mkdir temp_examples
for x in `cat examples/take_list`; do mv examples/$x temp_examples; done
rm -rf examples
mv temp_examples examples


cd .. # now in pads directory

echo cleaning padsc directory
mkdir temp_padsc
for x in `cat padsc/take_list`; do mv padsc/$x temp_padsc; done
rm -rf padsc
mv temp_padsc padsc

echo Adding licenses
# should be in pads directory
# must do this before cleaning scripts directory
scripts/release/make.notices.sh

echo cleaning scripts directory
mkdir temp_scripts
for x in `cat scripts/RELEASE_SCRIPTS`; do mv scripts/$x temp_scripts; done
rm -rf scripts
mv temp_scripts scripts


echo taring up the desired files
cd ..  # now above pads directory
echo building bundle
tar cfz pads.tar.gz `cat pads/take_list`
