# NMAKE_SETENV.sh is for use with sh/ksh/bash.
# From the same directory as NMAKE_SETENV.sh, do:
#     . ./NMAKE_SETENV.sh
#

if [ "$_pads_verbose"x == x ]; then
  _pads_verbose=1
fi

_status=OK 

echo " "

if [ "$_pads_verbose" != 0 ]; then
  echo " "
fi

if [ "$PADS_HOME"x == x ]; then
  echo "##############################################################################"
  echo "# Set env var PADS_HOME and then use NMAKE_SETENV.sh again."
  echo "##############################################################################"
  echo " "
  _status=FAILED
fi

if [ $_status == "OK" ]; then
  _pads_verbose_bak=$_pads_verbose
  _pads_verbose=0
  . $PADS_HOME/scripts/DO_SETENV.sh
  _pads_verbose=$_pads_verbose_bak
  _pads_verbose_bak=
fi

if [ $_status == "OK" ]; then
  ast_bin_dir=$AST_HOME/bin
  PATH=`echo ${ast_bin_dir}:${PATH} | $remove_dups`; export PATH

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
