#!/bin/tcsh

#
# This script removes then updates mamprobe and package
# (under ast-base/bin) so that the minor mods to these files
# that occur during the build of libast are undone.
#


if (! $?PADS_HOME) then
  echo "\n##############################################################################"
  echo "# Set env var PADS_HOME and then use again."
  echo "##############################################################################\n"
  exit 0
endif

cd $PADS_HOME/ast-base/bin
/bin/rm -f mamprobe package
cvs update mamprobe package >& /dev/null

