
 # This is a GNU makefile.

PADS_HOME = ../../..
ifndef AST_ARCH
  AST_ARCH := $(shell $(PADS_HOME)/ast-ast/bin/package.cvs)
  export AST_ARCH
endif

CURDIR := $(shell pwd)
ifneq ($(AST_ARCH),$(notdir $(CURDIR)))
include $(PADS_HOME)/mk/redirect.mk
else

 # The following rules are run from the arch directory

PADS_HOME = ../../../..
GEN_DIR = ../../gen
GEN_WRITE = 1

USE_GALAX = 1

# the following indicates that generated .c files should not be removed
.PRECIOUS: $(GEN_DIR)/%.c 

# need to set this before including rules.mk
REGRESS_TESTS = 1

include $(PADS_HOME)/mk/rules.mk

VPATH =  .. ../../p

XML_TESTS = test_children
ALL_TESTS_O = $(XML_TESTS)
ALL_TESTS_D = $(ALL_TESTS_O:%=%_d)
ALL_TESTS = $(ALL_TESTS_O) $(ALL_TESTS_D)

clean:
	$(RM) -f *~ *.o $(ALL_TESTS)

veryclean: clean
	$(RM) $(GEN_DIR)/*.[hc]

# This works for some but not all test cases

# These cases do not fit the usual pattern
test_children: $(GEN_DIR)/format7.c test_children.c $(INCLUDE_DEPS) $(LIB_DEPS_O)
	@$(CCExec_DYNAMIC_O)

test_children_d: $(GEN_DIR)/format7.c test_children.c $(INCLUDE_DEPS) $(LIB_DEPS_D)
	@$(CCExec_DYNAMIC_D)

 # End of rules that are run from the arch directory
endif
