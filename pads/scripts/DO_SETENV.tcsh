#!/bin/tcsh
# DO_SETENV.tcsh is for use with csh/tcsh
# From the same directory as DO_SETENV.tcsh, do:
#      source DO_SETENV.tcsh
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
  echo "# Set env var PADS_HOME and then use DO_SETENV.tcsh again."
  echo "##############################################################################"
  echo " "
  set _status = FAILED
endif

if ($_status == "OK") then
  if (! (-e $PADS_HOME/scripts/package)) then
    echo "##############################################################################"
    echo "# Invalid setting (?) : PADS_HOME = $PADS_HOME"
    echo "#"
    echo "# Cannot find $PADS_HOME/scripts/package"
    echo "#"
    echo "# Set env var PADS_HOME correctly and then use DO_SETENV.tcsh again."
    echo "##############################################################################"
    echo " "
    set _status = FAILED
  endif
endif

if ($_status == "OK") then

  setenv AST_ARCH `$PADS_HOME/scripts/package`

  if (! $?AST_HOME) then
    setenv AST_HOME /home/gsf/arch/$AST_ARCH
    if ($_pads_verbose != 0) then
      echo "##############################################################################"
      echo "# Setting env var AST_HOME to $AST_HOME"
      echo "# If you do not like this setting, set it to something else"
      echo "# and then use DO_SETENV.tcsh again."
      echo "##############################################################################"
      echo " "
    endif
  endif

  if (! $?INSTALLROOT) then
    setenv INSTALLROOT $PADS_HOME/arch/$AST_ARCH
    if ($_pads_verbose != 0) then
      echo "##############################################################################"
      echo "# Setting env var INSTALLROOT to $INSTALLROOT"
      echo "# If you do not like this setting, set it to something else"
      echo "# and then use DO_SETENV.tcsh again."
      echo "##############################################################################"
      echo " "
    endif
  endif

  if (! -e $INSTALLROOT) then
    (mkdir -p $INSTALLROOT >& /dev/null) || set _status = FAILED
  endif
  if (! -e $INSTALLROOT/bin) then
    (mkdir -p $INSTALLROOT/bin >& /dev/null) || set _status = FAILED
  endif
  if (! -e $INSTALLROOT/include) then
    (mkdir -p $INSTALLROOT/include >& /dev/null) || set _status = FAILED
  endif
  if (! -e $INSTALLROOT/lib) then
    (mkdir -p $INSTALLROOT/lib >& /dev/null) || set _status = FAILED
  endif
  if (! -e $INSTALLROOT/man) then
    (mkdir -p $INSTALLROOT/man >& /dev/null) || set _status = FAILED
  endif

  if ($_status == "FAILED") then
    echo "##############################################################################"
    echo "# WARNING: Could not create INSTALLROOT $INSTALLROOT"
    echo "# or one of its subdirs (bin, include, lib, man).  Correct problem (e.g.,"
    echo "# define another INSTALLROOT) and then use DO_SETENV.tcsh again."
    echo "##############################################################################"
    echo " "
  endif

  set ast_lib_dir  = $AST_HOME/lib
  set ast_man_dir  = $AST_HOME/man

  set pads_bin_dir    = $INSTALLROOT/bin
  set pads_lib_dir    = $INSTALLROOT/lib
  set pads_man_dir    = $INSTALLROOT/man
  set pads_script_dir = $PADS_HOME/scripts
  set remove_dups     = $pads_script_dir/removedups.pl

  if (! $?LD_LIBRARY_PATH) then
    setenv LD_LIBRARY_PATH ""
  endif
  if (! $?SHLIB_PATH) then
    setenv SHLIB_PATH ""
  endif
  if (! $?MANPATH) then
    setenv MANPATH ""
  endif

  setenv LD_LIBRARY_PATH `echo ${pads_lib_dir}:${ast_lib_dir}:${LD_LIBRARY_PATH} | $remove_dups`
  setenv SHLIB_PATH      `echo ${pads_lib_dir}:${ast_lib_dir}:${SHLIB_PATH} | $remove_dups`
  setenv MANPATH         `echo ${pads_man_dir}:${ast_man_dir}:${MANPATH} | $remove_dups`
  setenv PATH            `echo ${pads_bin_dir}:${pads_script_dir}:${PATH} | $remove_dups`

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
