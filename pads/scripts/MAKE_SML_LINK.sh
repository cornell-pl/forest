#!/bin/sh

_pads_status=OK 

echo " "
if [ "$PADS_HOME"x = x ]; then
  echo "##############################################################################"
  echo "# Set env var PADS_HOME and then use MAKE_SML_LINK.sh again."
  echo "##############################################################################"
  _pads_status=FAILED
fi

if [ "$SML_BIN_DIR"x = x ]; then
  echo "##############################################################################"
  echo "# Set env var SML_BIN_DIR to the SML bin directory"
  echo "#   and then use MAKE_SML_LINK.sh again."
  echo "##############################################################################"
  _pads_status=FAILED
fi

if [ $_pads_status = "OK" ]; then
  if [ ! -e $PADS_HOME ]; then
    echo "##############################################################################"
    echo "# PADS_HOME ($PADS_HOME) does not exist."
    echo "# Modify env var PADS_HOME and then use MAKE_SML_LINK.sh again."
    echo "##############################################################################"
    _pads_status=FAILED
  fi
fi

if [ $_pads_status = "OK" ]; then
  if [ ! -e $PADS_HOME/scripts/arch-n-opsys ]; then
    echo "##############################################################################"
    echo "# PADS_HOME ($PADS_HOME) not a valid PADS installation."
    echo "# Modify env var PADS_HOME and then use MAKE_SML_LINK.sh again."
    echo "##############################################################################"
    _pads_status=FAILED
  fi
fi

if [ $_pads_status = "OK" ]; then
  if [ ! -e $SML_BIN_DIR ]; then
    echo "##############################################################################"
    echo "# SML_BIN_DIR ($SML_BIN_DIR) does not exist."
    echo "# Modify env var SML_BIN_DIR to the SML bin directory"
    echo "#   and then use MAKE_SML_LINK.sh again."
    echo "##############################################################################"
    _pads_status=FAILED
  fi
fi

if [ $_pads_status = "OK" ]; then
  _arch_n_opsys=`$PADS_HOME/scripts/arch-n-opsys`
  _ast_arch=`$PADS_HOME/ast-ast/bin/package.cvs`
  if [ ! -e $SML_BIN_DIR/sml ]; then
    echo "##############################################################################"
    echo "# Bad SML_BIN_DIR ($SML_BIN_DIR) setting"
    echo "#   ( $SML_BIN_DIR/sml not found )"
    echo "# Modify env var SML_BIN_DIR to the SML bin directory"
    echo "#   and then use MAKE_SML_LINK.sh again."
    echo "##############################################################################"
  else
    /bin/rm -f $PADS_HOME/binlinks/sml.$_ast_arch
    echo "Creating symbolic link $SML_BIN_DIR/sml -> $PADS_HOME/binlinks/sml.$_ast_arch"
    ln -s $SML_BIN_DIR/sml $PADS_HOME/binlinks/sml.$_ast_arch
  fi
  if [ ! -e $SML_BIN_DIR/.run/run.$_arch_n_opsys ]; then
    echo "##############################################################################"
    echo "# Bad SML_BIN_DIR ($SML_BIN_DIR) setting"
    echo "#   ( $SML_BIN_DIR/.run/run.$_arch_n_opsys not found )"
    echo "# Modify env var SML_BIN_DIR to the SML bin directory"
    echo "#   and then use MAKE_SML_LINK.sh again."
    echo "##############################################################################"
  else
    /bin/rm -f $PADS_HOME/binlinks/smlruntime.$_ast_arch
    echo "Creating symbolic link $SML_BIN_DIR/.run/run.$_arch_n_opsys -> $PADS_HOME/binlinks/smlruntime.$_ast_arch"
    ln -s $SML_BIN_DIR/.run/run.$_arch_n_opsys $PADS_HOME/binlinks/smlruntime.$_ast_arch
  fi
  _arch_n_opsys=
  _ast_arch=
fi

echo " "
_pads_status=
