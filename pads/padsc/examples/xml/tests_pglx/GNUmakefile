
 # This is a GNU makefile.

ifndef PADS_HOME
  export PADS_HOME=$(shell cd ../../..; pwd)
endif

ifndef AST_ARCH
  export AST_ARCH=$(shell $(PADS_HOME)/ast-ast/bin/package.cvs)
endif

CURDIR := $(shell pwd)
ifneq ($(AST_ARCH),$(notdir $(CURDIR)))
include $(PADS_HOME)/mk/redirect.mk
else

 # The following rules are run from the arch directory

GEN_DIR = ..
GEN_WRITE = 1

# the following indicates that generated .c files should not be removed
.PRECIOUS: $(GEN_DIR)/%.c  $(GEN_DIR)/%_expanded.c %_expanded.c

# need to set this before including rules.mk
REGRESS_TESTS = 1

include $(PADS_HOME)/mk/rules.mk

VPATH =  ..

ifndef GEN_GALAX
ifndef USE_GALAX
GALAX_NONE = 1
endif
endif

ifdef GALAX_NONE
XML_TESTS =
else
XML_TESTS = test_children
endif

ALL_TESTS_O = $(WRITE_TESTS) $(XML_TESTS)
ALL_TESTS_D = $(ALL_TESTS_O:%=%_d)
ALL_TESTS = $(ALL_TESTS_O) $(ALL_TESTS_D)

CLEANUP_TESTS = $(ALL_TESTS)

regress: regress_o regress_d

regress_o:  sanity_check

	@echo " "; echo "DONE"; echo " "

regress_d:  sanity_check
	@echo " "; echo "DONE"; echo " "

all_tests: sanity_check $(ALL_TESTS)

sanity_check:
	@$(SanityCheck)

clean:
	$(RM) -f *~ *.o $(CLEANUP_TESTS)

veryclean: clean
	$(RM) $(GEN_DIR)/*.[hc] $(GEN_DIR)/*.xsd tmp/*

# This works for some but not all test cases

# These cases do not fit the usual pattern

ifdef GEN_DIR
endif

ifdef DEBUG_RULES_MK
	@echo "Using rules.mk rule R_DD"
endif
	@$(CCExec_DYNAMIC_D)

ifdef GALAX_NONE
test_children: var_error

test_children_d: var_error

var_error:
	@echo " "
	@echo "ERROR: to build test_children you must use one of the following"
	@echo "   gmake GEN_GALAX=1 <target>"
	@echo "   gmake USE_GALAX=1 <target>"
	@echo " "
	@echo "GEN_GALAX: use the libpglx with fake calls to Galax API"
	@echo "USE_GALAX: use the libpglx with real calls to Galax API"
	@echo " "
	@echo "(you must build libpglx with the same option for things to work properly)"
	@echo " "

else
endif

$(GEN_DIR)/http.c:
	$(PADS_HOME)/scripts/padsc -anone ../../p/http.p  -r ../../gen -I. -I..

 # End of rules that are run from the arch directory
endif
