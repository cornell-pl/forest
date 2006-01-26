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

include $(PADS_HOME)/mk/rules.mk

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

all: $(DEMOS)

clean:
	$(RM) -f *~ *.o $(DEMOS)

veryclean: clean
	$(RM) $(GEN_DIR)/*

 # End of rules that are run from the arch directory
endif
