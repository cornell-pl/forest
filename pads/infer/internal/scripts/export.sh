#!/bin/sh
# this script should be run from the directory pads/infer
# it will leave a tar ball at that level, and a directory pads_infer_remove_me
# that is the source of the tar ball.
#
# Notes: When putting out a new release, 
#  . Increment the learning version number in main.sml
#  . Be sure to update the take_lists in examples/{p,tests,data}
#  . Make sure compiler is set to generate release version of compiler
#  . Add tarball to pads-private/dist directory.

# pads-infer-1-00-a   pads 1.0  2007-12-17  initial release

#cvs -d :ext:cvs-graphviz.research.att.com:/cvsroot export -r $1 pads/infer
cvs -d :ext:cvs-graphviz.research.att.com:/cvsroot export -DNOW pads/infer
mv pads/infer infer
rmdir pads

bundlename=`echo $1 | awk -F "-" '{print $1"-"$2"."$3"."$4}'`
echo $bundlename


#in checked-out version oflearning system
cd infer 

echo Adding licenses
# should be in infer directory
# must do this before removing internal directory
internal/scripts/make.notices.sh

# remove internal directories/files
rm -rf internal

# clean example directory
cd examples
pwd
#clean examples/data directory
echo cleaning data directory
mkdir temp_data
for x in `cat data/RELEASE_DATA`; do cp data/$x temp_data; done
rm -rf data
mv temp_data data

cd ..
# now in infer directory

echo taring the desired files
cd ..  # now above checked out infer directory
echo building bundle
tar cfz $bundlename.tar.gz 

mv infer pads_infer_remove_me
