
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

CODE_DIR = ../gen

USE_GALAX = 1

# the following indicates that generated .c files should not be removed
.PRECIOUS: $(GEN_DIR)/%.c $(GEN_DIR)/%_expanded.c 

# need to give these rules before including rules.mk
REGRESS_TESTS = 1

INCLUDES += -I ../gen
CURDIR := $(shell pwd)
SCRIPTS = $(PADS_HOME)/scripts

include $(PADS_HOME)/mk/rules.mk

ifdef GALAXCOMPILE
CDBGFLAGS += -DGALAXCOMPILE
COPTFLAGS += -DGALAXCOMPILE
endif

# DEP_myenum =

.PHONY: current pglx_lib clean


current: test_smart_myenum-d

test_load_%-g: $(GEN_DIR)/%.c $(CODE_DIR)/test_load_%.c $(INCLUDE_DEPS) $(LIB_DEPS_D) 
	@$(CCExec_DYNAMIC_D)

test_load_%: $(GEN_DIR)/%.c $(CODE_DIR)/test_load_%.c $(INCLUDE_DEPS) $(LIB_DEPS_O)  
	@$(CCExec_DYNAMIC_O)

test_smart_%-d: $(GEN_DIR)/%_expanded.c $(CODE_DIR)/test_smart_%.c $(CODE_DIR)/test_smart.h $(INCLUDE_DEPS) $(LIB_DEPS_D)  
	@$(CCExec_DYNAMIC_D)

test_smart_%-g: $(GEN_DIR)/%.c $(CODE_DIR)/test_smart_%.c $(CODE_DIR)/test_smart.h $(INCLUDE_DEPS) $(LIB_DEPS_D)  
	@$(CCExec_DYNAMIC_D)

test_smart_%: $(GEN_DIR)/%.c $(CODE_DIR)/test_smart_%.c $(CODE_DIR)/test_smart.h $(INCLUDE_DEPS) $(LIB_DEPS_O)  
	@$(CCExec_DYNAMIC_O)

test_kth_child_%-g: $(GEN_DIR)/%.c $(CODE_DIR)/test_kth_child_%.c $(INCLUDE_DEPS) $(LIB_DEPS_D)  
	@$(CCExec_DYNAMIC_D)

test_kth_child_%: $(GEN_DIR)/%.c $(CODE_DIR)/test_kth_child_%.c $(INCLUDE_DEPS) $(LIB_DEPS_O)  
	@$(CCExec_DYNAMIC_O)

test_walk_%: $(GEN_DIR)/%.c $(CODE_DIR)/test_walk_%.c $(INCLUDE_DEPS) $(LIB_DEPS_O)
	@$(CCExec_DYNAMIC_O)

test_query_%-g: $(GEN_DIR)/%.c $(CODE_DIR)/test_query_%.c $(INCLUDE_DEPS) $(LIB_DEPS_D)
	@$(CCExec_DYNAMIC_D)

test_query_%: $(GEN_DIR)/%.c $(CODE_DIR)/test_query_%.c $(INCLUDE_DEPS) $(LIB_DEPS_O)
	@$(CCExec_DYNAMIC_O)

test_query_%-d: $(GEN_DIR)/%_expanded.c $(CODE_DIR)/test_query_%.c $(CODE_DIR)/test_query.h $(INCLUDE_DEPS) $(LIB_DEPS_D)  
	@$(CCExec_DYNAMIC_D)

test_rr_%-g: $(GEN_DIR)/%.c $(CODE_DIR)/test_rr_%.c $(INCLUDE_DEPS) $(LIB_DEPS_D)  
	@$(CCExec_DYNAMIC_D)

test_rr_%: $(GEN_DIR)/%.c $(CODE_DIR)/test_rr_%.c $(INCLUDE_DEPS) $(LIB_DEPS_O)  
	@$(CCExec_DYNAMIC_O)

#dogen : $(CODE_DIR)/enum_kth.c $(CODE_DIR)/enum_kth.h
#	@echo "Building generated source files"
#	$(SCRIPTS)/mksrc.pl $(CC) $(CODE_DIR)/enum_kth.c $(CODE_DIR) $(PADS_HOME)
#	touch dogen

pglx_lib:
	@(cd $(PADS_HOME)/padsc; $(MAKE) pglx_lib)

# MYENUM_DEPS = $(CODE_DIR)/enum_kth.c $(CODE_DIR)/enum_kth.h
MYENUM_DEPS = $(CODE_DIR)/enum_kth.h $(GEN_DIR)/enum.h $(GEN_DIR)/enum.c $(INCLUDE_DEPS)

$(GEN_DIR)/myenum.c: $(MYENUM_DEPS)
	touch $(GEN_DIR)/myenum.c


ifdef WITH_KTH
CDBGFLAGS += -DWITH_KTH
COPTFLAGS += -DWITH_KTH
endif


INCLUDES += -I ../gen
VPATH =  .. ../../p ../gen ../../gen

XML_TESTS = test_children 
ALL_TESTS_O = $(XML_TESTS)
ALL_TESTS_D = $(ALL_TESTS_O:%=%_d)
ALL_TESTS = $(ALL_TESTS_O) $(ALL_TESTS_D) test_load_myenum

clean:
	$(RM) -f *~ *.o $(ALL_TESTS)

veryclean: clean
	$(RM) $(GEN_DIR)/*.[hc]

# This works for some but not all test cases

# These cases do not fit the usual pattern
test_children: $(GEN_DIR)/format7.c test_children.c $(INCLUDE_DEPS) $(LIB_DEPS_O)
	@$(CCExec_DYNAMIC_O)

test_init: $(GEN_DIR)/format7.c test_init.c $(INCLUDE_DEPS) $(LIB_DEPS_O)
	@$(CCExec_DYNAMIC_O)

test_children_d: $(GEN_DIR)/format7.c test_children.c $(INCLUDE_DEPS) $(LIB_DEPS_D)
	@$(CCExec_DYNAMIC_D)

$(GEN_DIR)/myenum.c: enum.c enum.h enum_kth.c enum_kth.h

test_load_myenum: $(GEN_DIR)/myenum.c gen/test_load_myenum.c $(INCLUDE_DEPS) $(LIB_DEPS_O)
	@$(CCExec_DYNAMIC_O)


 # End of rules that are run from the arch directory
endif
