
 # This is a GNU makefile.

ifndef PADS_HOME
%: forceabort2
	@echo "ERROR: env variable PADS_HOME must be defined"
	@exit 1
forceabort2: ;
endif

ifndef AST_ARCH
  AST_ARCH := $(shell $(PADS_HOME)/ast-ast/bin/package.cvs)
  export AST_ARCH
endif

CURDIR := $(shell pwd)
ifneq ($(AST_ARCH),$(notdir $(CURDIR)))
include $(PADS_HOME)/mk/redirect.mk
else

 # The following rules are run from the arch directory

GEN_DIR = ../../gen
GEN_WRITE = 1

USE_GALAX = 1

# the following indicates that generated .c files should not be removed
.PRECIOUS: $(GEN_DIR)/%.c 

# need to give these rules before including rules.mk
REGRESS_TESTS = 1

test_load_%-g: $(GEN_DIR)/%.c gen/test_load_%.c $(INCLUDE_DEPS) $(LIB_DEPS_D)
	@$(CCExec_DYNAMIC_D)

test_load_%: $(GEN_DIR)/%.c gen/test_load_%.c $(INCLUDE_DEPS) $(LIB_DEPS_O)
	@$(CCExec_DYNAMIC_O)

test_walk_%: $(GEN_DIR)/%.c gen/test_walk_%.c $(INCLUDE_DEPS) $(LIB_DEPS_O)
	@$(CCExec_DYNAMIC_O)

test_query_%-g: $(GEN_DIR)/%.c gen/test_query_%.c $(INCLUDE_DEPS) $(LIB_DEPS_D)
	@$(CCExec_DYNAMIC_D)

test_query_%: $(GEN_DIR)/%.c gen/test_query_%.c $(INCLUDE_DEPS) $(LIB_DEPS_O)
	@$(CCExec_DYNAMIC_O)

include $(PADS_HOME)/mk/rules.mk
ifdef GALAXCOMPILE
CDBGFLAGS += -DGALAXCOMPILE
COPTFLAGS += -DGALAXCOMPILE
endif

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

test_kth_child_myenum: $(GEN_DIR)/myenum.c gen/test_kth_child_myenum.c $(INCLUDE_DEPS) $(LIB_DEPS_O)
	@$(CCExec_DYNAMIC_O)

 # End of rules that are run from the arch directory
endif
