# INT_DO_SETENV.sh is a helper script.
# See DO_SETENV.sh and Q_DO_SETENV.sh.

_pads_status=OK 

if [ "$_pads_verbose"x == x ]; then
  echo "##############################################################################"
  echo "# Do not use INT_DO_SETENV directly, use DO_SETENV or Q_DO_SETENV"
  echo "##############################################################################"
  echo " "
  _pads_status=FAILED
fi


if [ $_pads_status == "OK" ]; then
  if [ "$_pads_verbose" != 0 ]; then
    echo " "
  fi

  if [ "$PADS_HOME"x == x ]; then
    echo "##############################################################################"
    echo "# Set env var PADS_HOME and then use $_pads_do_prog again."
    echo "##############################################################################"
    echo " "
    _pads_status=FAILED
  fi
fi

if [ $_pads_status == "OK" ]; then
  if [ ! -e $PADS_HOME/ast-ast/bin/package.cvs ]; then
    echo "##############################################################################"
    echo "# Invalid setting (?) : PADS_HOME = $PADS_HOME"
    echo "#"
    echo "# Cannot find $PADS_HOME/ast-ast/bin/package.cvs"
    echo "#"
    echo "# Set env var PADS_HOME correctly and then use DO_SETENV.tcsh again."
    echo "##############################################################################"
    echo " "
    _pads_status=FAILED
  fi
fi

if [ $_pads_status == "OK" ]; then
  AST_ARCH=`$PADS_HOME/ast-ast/bin/package.cvs`; export AST_ARCH

  if [ "$AST_HOME"x == x ]; then
    AST_HOME=$PADS_HOME/ast-ast/arch/$AST_ARCH; export AST_HOME
    if [ "$_pads_verbose" != 0 ]; then
      echo "##############################################################################"
      echo "# Setting env var AST_HOME to $AST_HOME"
      echo "# If you do not like this setting, set it to something else"
      echo "# and then use $_pads_do_prog again."
      echo "##############################################################################"
      echo " "
    fi
  fi

  if [ "$INSTALLROOT"x == x ]; then
    INSTALLROOT=$PADS_HOME/ast-ast/arch/$AST_ARCH; export INSTALLROOT
    if [ "$_pads_verbose" != 0 ]; then
      echo "##############################################################################"
      echo "# Setting env var INSTALLROOT to $INSTALLROOT"
      echo "# If you do not like this setting, set it to something else"
      echo "# and then use $_pads_do_prog again."
      echo "##############################################################################"
      echo " "
    fi
  fi

  if [ "$OCAML_LIB"x == x ]; then
    OCAML_LIB=/usr/common/lib/ocaml; export OCAML_LIB
  fi
  if [ "$GALAX_HOME"x == x ]; then
    GALAX_HOME=/home/mff/Galax-rh7; export GALAX_HOME
  fi
  GALAX_LIB=$GALAX_HOME/lib/c; export GALAX_LIB

  if [ ! -e $INSTALLROOT ]; then
    (mkdir -p $INSTALLROOT > /dev/null 2>&1) || _pads_status=FAILED
  fi
  if [ ! -e $INSTALLROOT/bin ]; then
    (mkdir -p $INSTALLROOT/bin > /dev/null 2>&1) || _pads_status=FAILED
  fi
  if [ ! -e $INSTALLROOT/include ]; then
    (mkdir -p $INSTALLROOT/include > /dev/null 2>&1) || _pads_status=FAILED
  fi
  if [ ! -e $INSTALLROOT/lib ]; then
    (mkdir -p $INSTALLROOT/lib > /dev/null 2>&1) || _pads_status=FAILED
  fi
  if [ ! -e $INSTALLROOT/man ]; then
    (mkdir -p $INSTALLROOT/man > /dev/null 2>&1) || _pads_status=FAILED
  fi

  if [ $_pads_status == "FAILED" ]; then
    echo "##############################################################################"
    echo "# WARNING: Could not create INSTALLROOT $INSTALLROOT"
    echo "# or one of its subdirs (bin, include, lib, man).  Correct problem (e.g.,"
    echo "# define another INSTALLROOT) and then use DO_SETENV.tcsh again."
    echo "##############################################################################"
    echo " "
  fi

  ast_lib_dir=$AST_HOME/lib
  ast_man_dir=$AST_HOME/man

  pads_bin_dir=$INSTALLROOT/bin
  pads_lib_dir=$INSTALLROOT/lib
  pads_man_dir=$INSTALLROOT/man
  pads_script_dir=$PADS_HOME/scripts
  remove_dups=$pads_script_dir/removedups.pl

  LD_LIBRARY_PATH=`echo ${pads_lib_dir}:${ast_lib_dir}:${LD_LIBRARY_PATH} | $remove_dups`; export LD_LIBRARY_PATH
  SHLIB_PATH=`echo ${pads_lib_dir}:${ast_lib_dir}:${SHLIB_PATH} | $remove_dups`; export SHLIB_PATH
  MANPATH=`echo ${pads_man_dir}:${ast_man_dir}:${MANPATH} | $remove_dups`; export MANPATH
  PATH=`echo ${pads_bin_dir}:${pads_script_dir}:${PATH} | $remove_dups`; export PATH

  if [ -e $OCAML_LIB ]; then
    LD_LIBRARY_PATH=`echo ${LD_LIBRARY_PATH}:${OCAML_LIB} | $remove_dups`; export LD_LIBRARY_PATH
  fi
  if [ -e $GALAX_LIB ]; then
    LD_LIBRARY_PATH=`echo ${LD_LIBRARY_PATH}:${GALAX_LIB} | $remove_dups`; export LD_LIBRARY_PATH
  fi

  if [ "$_pads_use_nmake" != 0 ]; then
    ast_bin_dir=$AST_HOME/bin
     PATH=`echo ${ast_bin_dir}:${PATH} | $remove_dups`; export PATH
  fi

  if [ "$_pads_verbose" != 0 ]; then
    echo "PADS_HOME=$PADS_HOME"
    echo "INSTALLROOT=$INSTALLROOT"
    echo "AST_ARCH=$AST_ARCH"
    echo "AST_HOME=$AST_HOME"
    echo "LD_LIBRARY_PATH=$LD_LIBRARY_PATH"
    echo "SHLIB_PATH=$SHLIB_PATH"
    echo "MANPATH=$MANPATH"
    echo "PATH=$PATH"
    echo "OCAML_LIB=$OCAML_LIB"
    echo "GALAX_HOME=$GALAX_HOME"
    echo "GALAX_LIB=$GALAX_LIB"
    echo " "
  fi
fi
