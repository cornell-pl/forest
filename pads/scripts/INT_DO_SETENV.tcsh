#!/bin/tcsh
# INT_DO_SETENV.tcsh is a helper script.
# See DO_SETENV.tcsh and Q_DO_SETENV.tcsh.

set _pads_status = OK

if (! $?_pads_verbose) then
  echo "##############################################################################"
  echo "# Do not use INT_DO_SETENV directly, use DO_SETENV or Q_DO_SETENV"
  echo "##############################################################################"
  echo " "
  set _pads_status = FAILED
endif 

if ($_pads_status == OK) then
  if ($_pads_verbose != 0) then
    echo " "
  endif
  if (! $?PADS_HOME) then
    echo "##############################################################################"
    echo "# Set env var PADS_HOME and then use $_pads_do_prog again."
    echo "##############################################################################"
    echo " "
    set _pads_status = FAILED
  endif
endif

if ($_pads_status == "OK") then
  if (! (-e $PADS_HOME/ast-base/bin/package)) then
    echo "##############################################################################"
    echo "# Invalid setting (?) : PADS_HOME = $PADS_HOME"
    echo "#"
    echo "# Cannot find $PADS_HOME/ast-base/bin/package"
    echo "#"
    echo "# Set env var PADS_HOME correctly and then use $_pads_do_prog again."
    echo "##############################################################################"
    echo " "
    set _pads_status = FAILED
  endif
endif

if ($_pads_status == "OK") then

  setenv AST_ARCH `$PADS_HOME/ast-base/bin/package`

  if (! $?AST_HOME) then
    setenv AST_HOME $PADS_HOME/ast-base/arch/$AST_ARCH
    if ($_pads_verbose != 0) then
      echo "##############################################################################"
      echo "# Setting env var AST_HOME to $AST_HOME"
      echo "# If you do not like this setting, set it to something else"
      echo "# and then use $_pads_do_prog again."
      echo "##############################################################################"
      echo " "
    endif
  endif

  if (! $?INSTALLROOT) then
    setenv INSTALLROOT $PADS_HOME/ast-base/arch/$AST_ARCH
    if ($_pads_verbose != 0) then
      echo "##############################################################################"
      echo "# Setting env var INSTALLROOT to $INSTALLROOT"
      echo "# If you do not like this setting, set it to something else"
      echo "# and then use $_pads_do_prog again."
      echo "##############################################################################"
      echo " "
    endif
  endif

  if (! -e $INSTALLROOT) then
    (mkdir -p $INSTALLROOT >& /dev/null) || set _pads_status = FAILED
  endif
  if (! -e $INSTALLROOT/bin) then
    (mkdir -p $INSTALLROOT/bin >& /dev/null) || set _pads_status = FAILED
  endif
  if (! -e $INSTALLROOT/include) then
    (mkdir -p $INSTALLROOT/include >& /dev/null) || set _pads_status = FAILED
  endif
  if (! -e $INSTALLROOT/lib) then
    (mkdir -p $INSTALLROOT/lib >& /dev/null) || set _pads_status = FAILED
  endif
  if (! -e $INSTALLROOT/man) then
    (mkdir -p $INSTALLROOT/man >& /dev/null) || set _pads_status = FAILED
  endif

  if ($_pads_status == "FAILED") then
    echo "##############################################################################"
    echo "# WARNING: Could not create INSTALLROOT $INSTALLROOT"
    echo "# or one of its subdirs (bin, include, lib, man).  Correct problem (e.g.,"
    echo "# define another INSTALLROOT) and then use $_pads_do_prog again."
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
  if (! $?OCAML_LIB) then
    setenv OCAML_LIB /usr/common/lib/ocaml
  endif

  setenv LD_LIBRARY_PATH `echo ${pads_lib_dir}:${ast_lib_dir}:${LD_LIBRARY_PATH} | $remove_dups`
  setenv SHLIB_PATH      `echo ${pads_lib_dir}:${ast_lib_dir}:${SHLIB_PATH} | $remove_dups`
  setenv MANPATH         `echo ${pads_man_dir}:${ast_man_dir}:${MANPATH} | $remove_dups`
  setenv PATH            `echo ${pads_bin_dir}:${pads_script_dir}:${PATH} | $remove_dups`

  if (-d $OCAML_LIB) then
    setenv LD_LIBRARY_PATH `echo ${LD_LIBRARY_PATH}:${OCAML_LIB} | $remove_dups`
  endif

  if ($_pads_use_nmake == 1) then
    set ast_bin_dir  = $AST_HOME/bin
    setenv PATH `echo ${ast_bin_dir}:${PATH} | $remove_dups`
  endif

  # on linux, figure out where ligcc_s.so is using /usr/common/bin/gcc (if it exists)
  # and add that dir to LD_LIBRARY_PATH.  This is required because
  # gsf builds libast using /usr/common/bin/gcc.
  #  switch ("`uname -a`")
  #    case *Linux*:
  #    case *linux*:
  #      if (-e /usr/common/bin/gcc) then
  #        set _foo = `/usr/common/bin/gcc -print-file-name=libgcc_s.so`
  #        set _bar = $_foo:h
  #        set _zot = `(cd $_bar; pwd)`
  #        setenv LD_LIBRARY_PATH `echo ${LD_LIBRARY_PATH}:${_zot} | $remove_dups`
  #        unset _foo
  #        unset _bar
  #        unset _zot
  #        endif
  #      breaksw
  #  endsw

  if ($_pads_verbose != 0) then
    echo "PADS_HOME=$PADS_HOME"
    echo "INSTALLROOT=$INSTALLROOT"
    echo "AST_ARCH=$AST_ARCH"
    echo "AST_HOME=$AST_HOME"
    echo "LD_LIBRARY_PATH=$LD_LIBRARY_PATH"
    echo "SHLIB_PATH=$SHLIB_PATH"
    echo "MANPATH=$MANPATH"
    echo "PATH=$PATH"
    echo "OCAML_LIB=$OCAML_LIB"
    echo " "
  endif

endif
