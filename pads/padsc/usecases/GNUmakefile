
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

GEN_DIR = ../../gen
GEN_WRITE = 1

# Variables used for timing tests
DATA_DIR = ../../data
TIMING_DIR=../../timing
TIMING_SCR=$(PADS_HOME)/scripts/timing.sh
TFORMATS=dns ai
RMASKS=CaS SyS Ig

# the following indicates that generated files should not be removed
.PRECIOUS: %.o $(GEN_DIR)/%.c $(GEN_DIR)/%_expanded.c %_expanded.c \
	$(foreach mask,$(RMASKS),time_read_$(mask)_%)

# need to set this before including rules.mk
REGRESS_TESTS = 1

include $(PADS_HOME)/mk/rules.mk

VPATH =  .. ../../p

ifndef GEN_GALAX
ifndef USE_GALAX
GALAX_NONE = 1
endif
endif

OTHER_TESTS  = test_array test_array_string

ALL_TESTS_O = $(OTHER_TESTS) $(XML_TESTS)
ALL_TESTS_D = $(ALL_TESTS_O:%=%_d)
ALL_TESTS = $(ALL_TESTS_O) $(ALL_TESTS_D)

# do not build MISC tests when building 'all'
CLEANUP_TESTS = $(ALL_TESTS)

usecases_set: regress

regress:  sanity_check regress_test_fmt_simple \

	@echo " "; echo "DONE"; echo " "

other_tests: sanity_check $(OTHER_TESTS)
all_tests: sanity_check $(ALL_TESTS)

sanity_check:
	@$(SanityCheck)

regress_test_fmt_simple: simple/src/test_fmt_simple
	@(args=""; suf=""; $(RegressDef))

clean:
	$(RM) -f *~ *.o $(CLEANUP_TESTS)

veryclean: clean
	$(RM) $(GEN_DIR)/*.[hc] $(GEN_DIR)/*.xsd tmp/*

# This works for some but not all test cases

# These cases do not fit the usual pattern

.PHONY: time_% process_% do_time_pglx

time_read_SyS_%_d: CDBGFLAGS += -D 'READ_MASK=(P_SynCheck|P_Set)' -D 'E_REP_LEV=PerrorRep_None'
time_read_SyS_%_d: $(GEN_DIR)/%.o time_read_%.c $(INCLUDE_DEPS) $(LIB_DEPS_D)
	@$(CCExec_DYNAMIC_D)

time_read_SyS_%: COPTFLAGS += -D 'READ_MASK=(P_SynCheck|P_Set)' -D 'E_REP_LEV=PerrorRep_None'
time_read_SyS_%: $(GEN_DIR)/%.o time_read_%.c $(INCLUDE_DEPS) $(LIB_DEPS_O)
	@$(CCExec_DYNAMIC_O)

time_read_Ig_%_d: CDBGFLAGS += -D 'READ_MASK=(P_Ignore)' -D 'E_REP_LEV=PerrorRep_None'
time_read_Ig_%_d: $(GEN_DIR)/%.o time_read_%.c $(INCLUDE_DEPS) $(LIB_DEPS_D)
	@$(CCExec_DYNAMIC_D)

time_read_Ig_%: COPTFLAGS += -D 'READ_MASK=(P_Ignore)' -D "E_REP_LEV=PerrorRep_None"
time_read_Ig_%: $(GEN_DIR)/%.o time_read_%.c $(INCLUDE_DEPS) $(LIB_DEPS_O)
	@$(CCExec_DYNAMIC_O)

time_read_CaS_%_d: CDBGFLAGS += -D 'READ_MASK=(P_CheckAndSet)' -D 'E_REP_LEV=PerrorRep_None'
time_read_CaS_%_d: $(GEN_DIR)/%.o time_read_%.c $(INCLUDE_DEPS) $(LIB_DEPS_D)
	@$(CCExec_DYNAMIC_D)

time_read_CaS_%: COPTFLAGS += -D 'READ_MASK=(P_CheckAndSet)' -DE_REP_LEV=PerrorRep_None
time_read_CaS_%: $(GEN_DIR)/%.o time_read_%.c $(INCLUDE_DEPS) $(LIB_DEPS_O)
	@$(CCExec_DYNAMIC_O)

ifeq ($(PGLX_KIND),bulk)
COPTFLAGS += -D PGLX_BULK -DE_REP_LEV=PerrorRep_None
endif
ifeq ($(PGLX_KIND),linear)
COPTFLAGS += -D PGLX_LINEAR -DE_REP_LEV=PerrorRep_None
endif
ifeq ($(PGLX_KIND),smart)
COPTFLAGS += -D PGLX_SMART -DE_REP_LEV=PerrorRep_None
endif

pglx_bulk_% pglx_linear_% pglx_smart_%: \
$(GEN_DIR)/%.o time_pglx_%.c $(INCLUDE_DEPS) $(LIB_DEPS_O)
	@$(CCExec_DYNAMIC_O)

ifdef DEBUG_RULES_MK
	@echo "Using rules.mk rule R_DD"
endif
	@$(CCExec_DYNAMIC_D)

$(GEN_DIR)/http.c:
	$(PADS_HOME)/scripts/padsc -anone ../../p/http.p  -r ../../gen -I. -I..

 # End of rules that are run from the arch directory
endif
