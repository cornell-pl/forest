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
GEN_DIR =../../gen
GEN_WRITE = 1

CODE_DIR =../
USE_GALAX = 1

VPATH = ../../../p

# the following indicates that generated .c files should not be removed
.PRECIOUS: $(GEN_DIR)/%.c $(GEN_DIR)/%_expanded.c 

# need to give these rules before including rules.mk
REGRESS_TESTS = 1

CURDIR := $(shell pwd)
SCRIPTS = $(PADS_HOME)/scripts

include $(PADS_HOME)/mk/rules.mk

# Extend the INCLUDES after including rules.mk
TEMPL_DIR=$(PADS_HOME)/padsc/include/template
INCLUDES += -I$(TEMPL_DIR)

ifdef GALAXCOMPILE
CDBGFLAGS += -DGALAXCOMPILE
COPTFLAGS += -DGALAXCOMPILE
endif

# DEP_myenum =

LOAD_TESTS=test_load_format7 test_load_ai test_load_dns #test_load_dibbler_notrailer
BULK_QUERY_TESTS=test_bulk_format7 test_bulk_ai test_bulk_dns #test_bulk_dibbler_notrailer 
SMART_QUERY_TESTS=test_smart_format7 test_smart_ai #test_smart_dibbler_notrailer 
REGRESS_TESTS=regress_format7 regress_ai regress_dns #regress_dibbler_notrailer 

ALL_TESTS_O = $(LOAD_TESTS) $(BULK_QUERY_TESTS) $(SMART_QUERY_TESTS)
ALL_TESTS_D = $(ALL_TESTS_O:%=%_d)
ALL_TESTS = $(ALL_TESTS_O) $(ALL_TESTS_D)

.PHONY: pglx_lib clean

# Test bulk load
test_load_%-d: $(GEN_DIR)/%.c $(CODE_DIR)/test_load_%.c $(TEMPL_DIR)/pglx_load.h $(INCLUDE_DEPS) $(LIB_DEPS_D) 
	@$(CCExec_DYNAMIC_D)

test_load_%: $(GEN_DIR)/%.c $(CODE_DIR)/test_load_%.c $(TEMPL_DIR)/pglx_load.h $(INCLUDE_DEPS) $(LIB_DEPS_O)  
	@$(CCExec_DYNAMIC_O)

# Test bulk load & query
test_bulk_%-d: $(GEN_DIR)/%_expanded.c $(CODE_DIR)/test_bulk_%.c $(TEMPL_DIR)/pglx_bulk_query.h $(INCLUDE_DEPS) $(LIB_DEPS_D)  
	@$(CCExec_DYNAMIC_D)

test_bulk_%-d: $(GEN_DIR)/%.c $(CODE_DIR)/test_bulk_%.c $(TEMPL_DIR)/pglx_bulk_query.h $(INCLUDE_DEPS) $(LIB_DEPS_D)
	@$(CCExec_DYNAMIC_D)

test_bulk_%: $(GEN_DIR)/%.c $(CODE_DIR)/test_bulk_%.c $(TEMPL_DIR)/pglx_bulk_query.h $(INCLUDE_DEPS) $(LIB_DEPS_O)
	@$(CCExec_DYNAMIC_O)

# Test smart load & query
test_smart_%-d: $(GEN_DIR)/%_expanded.c $(CODE_DIR)/test_smart_%.c $(TEMPL_DIR)/pglx_smart_query.h $(INCLUDE_DEPS) $(LIB_DEPS_D)  
	@$(CCExec_DYNAMIC_D)

test_smart_%-d: $(GEN_DIR)/%.c $(CODE_DIR)/test_smart_%.c $(TEMPL_DIR)/pglx_smart_query.h $(INCLUDE_DEPS) $(LIB_DEPS_D)  
	@$(CCExec_DYNAMIC_D)

test_smart_%: $(GEN_DIR)/%.c $(CODE_DIR)/test_smart_%.c $(TEMPL_DIR)/pglx_smart_query.h $(INCLUDE_DEPS) $(LIB_DEPS_O)  
	@$(CCExec_DYNAMIC_O)

# Miscellaneous tests
test_kth_child_%: $(GEN_DIR)/%.c $(CODE_DIR)/test_kth_child_%.c $(INCLUDE_DEPS) $(LIB_DEPS_O)  
	@$(CCExec_DYNAMIC_O)

test_kth_child_%-d: $(GEN_DIR)/%.c $(CODE_DIR)/test_kth_child_%.c $(INCLUDE_DEPS) $(LIB_DEPS_D)  
	@$(CCExec_DYNAMIC_D)

test_rr_%: $(GEN_DIR)/%.c $(CODE_DIR)/test_rr_%.c $(INCLUDE_DEPS) $(LIB_DEPS_O)  
	@$(CCExec_DYNAMIC_O)

test_rr_%-d: $(GEN_DIR)/%.c $(CODE_DIR)/test_rr_%.c $(INCLUDE_DEPS) $(LIB_DEPS_D)  
	@$(CCExec_DYNAMIC_D)

pglx_lib:
	@(cd $(PADS_HOME)/padsc; $(MAKE) pglx_lib)

# REGRESSION TESTS
compare_files:
	if [[ -f $${S} && -f $${T} && -z "`cmp $${S} $${T}`" ]]; then echo "*** OK: $${N} PASSED"; else echo "*** ERROR: $${N} FAILED"; fi;
regress_load_%: test_load_% 
	./$? >../../xml/$?.xml	
	$(MAKE) -f ../GNUmakefile -k -s N=test_load_$* S=../../xml/test_load_$*.xml T=../../regress/test_$*.xml compare_files

regress_bulk_%: test_bulk_% 
	./$? >../../xml/$?.xml 
	$(MAKE) -f ../GNUmakefile -k -s N=test_bulk_$* S=../../xml/test_bulk_$*.xml T=../../regress/test_$*.xml compare_files

regress_smart_%: test_smart_% 
	./$? >../../xml/$?.xml 
	$(MAKE) -f ../GNUmakefile -k -s N=test_smart_$* S=../../xml/test_smart_$*.xml T=../../regress/test_$*.xml compare_files

regress_format7: regress_load_format7 regress_bulk_format7 regress_smart_format7

regress_ai: regress_load_ai regress_bulk_ai regress_smart_ai

regress_dibbler_notrailer: regress_load_dibbler_notrailer regress_bulk_dibbler_notrailer regress_smart_dibbler_notrailer

regress_dns: regress_load_dns regress_bulk_dns

regress: $(REGRESS_TESTS)

clean:
	$(RM) -f *~ *.o $(ALL_TESTS)

veryclean: clean
	$(RM) $(GEN_DIR)/*.[hc]

 # End of rules that are run from the arch directory
endif
