#!/bin/tcsh
# NMAKE_SETENV.tcsh is for use with csh/tcsh
# From the same directory as NMAKE_SETENV.tcsh, do:
#      source NMAKE_SETENV.tcsh
#

if (! $?_pads_verbose) then
  set _pads_verbose = 1
endif

set _status = OK

if ($_pads_verbose != 0) then
  echo " "
endif

if (! $?PADS_HOME) then
  echo "##############################################################################"
  echo "# Set env var PADS_HOME and then use NMAKE_SETENV.tcsh again."
  echo "##############################################################################"
  echo " "
  set _status = FAILED
endif

if ($_status == "OK") then
  set _pads_verbose_bak = $_pads_verbose
  set _pads_verbose = 0
  source $PADS_HOME/scripts/DO_SETENV.tcsh
  set _pads_verbose = $_pads_verbose_bak
  unset _pads_verbose_bak
endif

if ($_status == "OK") then
  set ast_bin_dir  = $AST_HOME/bin
  setenv PATH `echo ${ast_bin_dir}:${PATH} | $remove_dups`

  if ($_pads_verbose != 0) then
    echo "PADS_HOME=$PADS_HOME"
    echo "INSTALLROOT=$INSTALLROOT"
    echo "AST_ARCH=$AST_ARCH"
    echo "AST_HOME=$AST_HOME"
    echo "LD_LIBRARY_PATH=$LD_LIBRARY_PATH"
    echo "SHLIB_PATH=$SHLIB_PATH"
    echo "MANPATH=$MANPATH"
    echo "PATH=$PATH"
    echo " "
  endif

endif
