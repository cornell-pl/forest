
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

OTHER_TESTS  = test_record_enum test_typedef \
               test_enum test_from
ALL_TESTS_O = $(WRITE_TESTS) $(OTHER_TESTS) $(XML_TESTS)
ALL_TESTS_D = $(ALL_TESTS_O:%=%_d)
ALL_TESTS = $(ALL_TESTS_O) $(ALL_TESTS_D)

# do not build MISC tests when building 'all'
MISC_TESTS_O = test_ios 
MISC_TESTS_D = $(MISC_TESTS_O:%=%_d)
MISC_TESTS = $(MISC_TESTS_O) $(MISC_TESTS_D)
CLEANUP_TESTS = $(ALL_TESTS) $(MISC_TESTS)

rcount:
	@(cd ..; $(MAKE) regress) | $(PADS_HOME)/scripts/count_differs.pl

regress: regress_o regress_d

regress_o:  sanity_check \
          regress_test_opt_o

	@echo " "; echo "DONE"; echo " "

regress_d:  sanity_check \
          regress_test_opt_d
	@echo " "; echo "DONE"; echo " "

other_tests: sanity_check $(OTHER_TESTS)
all_tests: sanity_check $(ALL_TESTS)

sanity_check:
	@$(SanityCheck)

regress_test_opt_o: test_opt
	@(args=""; suf=""; $(RegressDef))
regress_test_opt_d: test_opt_d
	@(args=""; suf=""; $(RegressDef))

regress_test_ios_o: test_ios
	@(args=""; input="../../data/small_ios_config"; suf=""; pp="$(PADS_HOME)/scripts/ios_pp.pl"; $(RegressInputPP))
regress_test_ios_d: test_ios_d
	@(args=""; input="../../data/small_ios_config"; suf=""; pp="$(PADS_HOME)/scripts/ios_pp.pl"; $(RegressInputPP))

clean:
	$(RM) -f *~ *.o $(CLEANUP_TESTS)

veryclean: clean
	$(RM) $(GEN_DIR)/*.[hc] $(GEN_DIR)/*.xsd tmp/*

# This works for some but not all test cases

# These cases do not fit the usual pattern

pads_count_lines: pads_count_lines.c $(INCLUDE_DEPS) $(LIB_DEPS_O)
	@$(CCExec_DYNAMIC_O)
pads_count_lines_d: pads_count_lines.c $(INCLUDE_DEPS) $(LIB_DEPS_D)
	@$(CCExec_DYNAMIC_D)

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
