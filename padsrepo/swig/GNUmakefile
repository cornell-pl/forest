# This makefile was cribbed from $PADS_HOME/demo and extended to
# show how one might build a SWIG interface to PADS code.  
# Noteworthy lines/sections are commented with the word "SWIG".

 # This makefile uses gmake extensions
 # such as $(shell ...) to execute a command

.PRECIOUS: %.c

# These variables need to be set
ifndef PADS_HOME
  export PADS_HOME=$(shell cd ..; pwd)
endif
ifndef AST_ARCH
  export AST_ARCH=$(shell $(PADS_HOME)/ast-ast/bin/package.cvs)
endif

# These variables should not need to change
PADSC=$(PADS_HOME)/scripts/padsc

CURDIR := $(shell pwd)
ifneq ($(AST_ARCH),$(notdir $(CURDIR)))
include $(PADS_HOME)/mk/redirect.mk
else

 # The following rules are run from the arch directory

GEN_DIR = ../gen
GEN_WRITE = 1

# the following indicates that generated files should not be removed
.PRECIOUS: %.o $(GEN_DIR)/%.c $(GEN_DIR)/%_expanded.c %_expanded.c


# SWIG: Because of how GNUmake matches pattern rules, SWIGrules.mk must
# be included before rules.mk in order to "override" some rules.

include $(PADS_HOME)/swig/SWIGrules.mk
include $(PADS_HOME)/mk/rules.mk
mam_cc_DLL=-D_BLD_DLL -fPIC
mam_cc_DLLBIG=-D_BLD_DLL -fPIC
mam_cc_PIC=-fPIC
mam_cc_PICBIG=-fPIC


VPATH = ..

# simple rules for our demo programs
%-accum: $(GEN_DIR)/%.c %-accum.c $(INCLUDE_DEPS) $(LIB_DEPS_O)
	@$(CCExec_DYNAMIC_O)

%-filter: $(GEN_DIR)/%.c %-filter.c $(INCLUDE_DEPS) $(LIB_DEPS_O)
	@$(CCExec_DYNAMIC_O)

%-fmt: $(GEN_DIR)/%.c %-fmt.c $(INCLUDE_DEPS) $(LIB_DEPS_O)
	@$(CCExec_DYNAMIC_O)

%-sum: $(GEN_DIR)/%.c %-sum.c $(INCLUDE_DEPS) $(LIB_DEPS_O)
	@$(CCExec_DYNAMIC_O)

%-query: $(GEN_DIR)/%.c %-query.c $(INCLUDE_DEPS) $(LIB_DEPS_O)
	@$(CCExec_DYNAMIC_O)

%-xml: $(GEN_DIR)/%.c %-xml.c $(INCLUDE_DEPS) $(LIB_DEPS_O)
	@$(CCExec_DYNAMIC_O)

DEMOS = wsl-xml wsl-accum wsl-fmt wsl-sum \
     sirius-xml sirius-accum sirius-fmt sirius-filter
# XXX NOT CHECKED IN YET ???
# go-xml go-query go-accum

#
# SWIG EXAMPLe: ai_accum.pl is a script that runs the accumulator for
# the ai format on the ai data.  It depends on two prerequisites, libai.so
# and libpads.so (not the original libpads.so).  See SWIGRules.mk for
# details on how they are made.
#
.PHONY:	ai_accum.pl

ai_accum.pl:	libai.so libpads.so

all: ai_accum.pl

# END SWIG EXAMPLE

clean:
	$(RM) -f *~ *.o $(DEMOS)

veryclean: clean
	$(RM) $(GEN_DIR)/*

 # End of rules that are run from the arch directory
endif
