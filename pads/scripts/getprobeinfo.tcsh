#!/bin/tcsh

source $PADS_HOME/scripts/DO_SETENV.tcsh
set echo
set targ = $PADS_HOME/mk/rules.arch.$AST_ARCH.mk
if (! -e $targ) then
  set probe_dir = $AST_HOME/lib/probe/C/mam
  set probe_file = `grep -l mam_cc_DEBUG $probe_dir/* | head -1`
  # echo "probe_file = $probe_file"
  # cat $probe_file | grep -v 'generated by' | sed -e 's/setv //' -e 's/$/ '\''/' -e 's/CC.SHARED.REGISTRY.PATH/mam_cc_SHARED_REGISTRY_PATH/g' -e 's/$(BINDIR:P=R=$(DLLDIR))/..\/lib/' -e 's/\\\$/\$\$/g' | sed -e 's/ /='\''/' | sed -e 's/ '\''/'\''/' > $targ
  cat $probe_file | grep -v 'generated by' | sed -e 's/setv //' -e 's/$/ /' -e 's/CC.SHARED.REGISTRY.PATH/mam_cc_SHARED_REGISTRY_PATH/g' -e 's/$(BINDIR:P=R=$(DLLDIR))/..\/lib/' -e 's/\\\$\([^ ]*\)/'\''\$\$\1'\''/g' | sed -e 's/ /=/' | sed -e 's/ $//' > $targ
endif
echo done
