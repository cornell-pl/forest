#!/bin/tcsh
# NMAKE_SETENV.tcsh is for use with csh/tcsh
# From the same directory as NMAKE_SETENV.tcsh, do:
#      source NMAKE_SETENV.tcsh
#

set _pads_status = OK
set _pads_do_prog = NMAKE_SETENV.tcsh

if (! $?PADS_HOME) then
  echo "##############################################################################"
  echo "# Set env var PADS_HOME and then use $_pads_do_prog again."
  echo "##############################################################################"
  echo " "
  set _status = FAILED
endif

if ($_pads_status == "OK") then
  set _pads_verbose = 0
  set _pads_use_nmake = 1
  source $PADS_HOME/scripts/INT_DO_SETENV.tcsh
  unset _pads_use_nmake
  unset _pads_verbose
endif

unset _pads_do_prog
unset _pads_status
