# DO_SETENV.sh is for use with sh/ksh/bash.
# From the same directory as DO_SETENV.sh, do:
#     . ./DO_SETENV.sh
#

if [ "$_pads_verbose"x == x ]; then
  _pads_verbose=1
fi

_status=OK 

if [ "$_pads_verbose" != 0 ]; then
  echo " "
fi

if [ "$PADS_HOME"x == x ]; then
  echo "##############################################################################"
  echo "# Set env var PADS_HOME and then use DO_SETENV.sh again."
  echo "##############################################################################"
  echo " "
  _status=FAILED
fi

if [ $_status == "OK" ]; then
  if [ ! -e $PADS_HOME/scripts/package ]; then
    echo "##############################################################################"
    echo "# Invalid setting (?) : PADS_HOME = $PADS_HOME"
    echo "#"
    echo "# Cannot find $PADS_HOME/scripts/package"
    echo "#"
    echo "# Set env var PADS_HOME correctly and then use DO_SETENV.tcsh again."
    echo "##############################################################################"
    echo " "
    _status=FAILED
  fi
fi

if [ $_status == "OK" ]; then
  AST_ARCH=`$PADS_HOME/scripts/package`; export AST_ARCH

  if [ "$AST_HOME"x == x ]; then
    AST_HOME=/home/gsf/arch/$AST_ARCH; export AST_HOME
    if [ "$_pads_verbose" != 0 ]; then
      echo "##############################################################################"
      echo "# Setting env var AST_HOME to $AST_HOME"
      echo "# If you do not like this setting, set it to something else"
      echo "# and then use DO_SETENV.sh again."
      echo "##############################################################################"
      echo " "
    fi
  fi

  if [ "$INSTALLROOT"x == x ]; then
    INSTALLROOT=$PADS_HOME/arch/$AST_ARCH; export INSTALLROOT
    if [ "$_pads_verbose" != 0 ]; then
      echo "##############################################################################"
      echo "# Setting env var INSTALLROOT to $INSTALLROOT"
      echo "# If you do not like this setting, set it to something else"
      echo "# and then use DO_SETENV.sh again."
      echo "##############################################################################"
      echo " "
    fi
  fi

  if [ ! -e $INSTALLROOT ]; then
    (mkdir -p $INSTALLROOT > /dev/null 2>&1) || _status=FAILED
  fi
  if [ ! -e $INSTALLROOT/bin ]; then
    (mkdir -p $INSTALLROOT/bin > /dev/null 2>&1) || _status=FAILED
  fi
  if [ ! -e $INSTALLROOT/include ]; then
    (mkdir -p $INSTALLROOT/include > /dev/null 2>&1) || _status=FAILED
  fi
  if [ ! -e $INSTALLROOT/lib ]; then
    (mkdir -p $INSTALLROOT/lib > /dev/null 2>&1) || _status=FAILED
  fi
  if [ ! -e $INSTALLROOT/man ]; then
    (mkdir -p $INSTALLROOT/man > /dev/null 2>&1) || _status=FAILED
  fi

  if [ $_status == "FAILED" ]; then
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

  if [ "$_pads_verbose" != 0 ]; then
    echo "PADS_HOME=$PADS_HOME"
    echo "INSTALLROOT=$INSTALLROOT"
    echo "AST_ARCH=$AST_ARCH"
    echo "AST_HOME=$AST_HOME"
    echo "LD_LIBRARY_PATH=$LD_LIBRARY_PATH"
    echo "SHLIB_PATH=$SHLIB_PATH"
    echo "MANPATH=$MANPATH"
    echo "PATH=$PATH"
    echo " "
  fi

fi
