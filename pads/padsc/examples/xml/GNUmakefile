
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

# Uncomment the following to use libpglx / the -x option
GEN_GALAX = 1

# the following indicates that generated .c files should not be removed
.PRECIOUS: $(GEN_DIR)/%.c 

# need to give these rules before including rules.mk

test_%-g: $(GEN_DIR)/%.c test_%.c $(INCLUDE_DEPS) $(LIB_DEPS_D)
	@$(CCExec_DYNAMIC_D)

test_%: $(GEN_DIR)/%.c test_%.c $(INCLUDE_DEPS) $(LIB_DEPS_O)
	@$(CCExec_DYNAMIC_O)

include $(PADS_HOME)/mk/rules.mk

VPATH =  .. ../../p

XML_TESTS = test_children
ALL_TESTS_O = $(XML_TESTS)
ALL_TESTS_D = $(ALL_TESTS_O:%=%-g)
ALL_TESTS = $(ALL_TESTS_O) $(ALL_TESTS_D)

clean:
	$(RM) -f *~ *.o $(ALL_TESTS)

veryclean: clean
	$(RM) $(GEN_DIR)/*.[hc]

# This works for some but not all test cases

# These cases do not fit the usual pattern
test_children: $(GEN_DIR)/format7.c test_children.c $(INCLUDE_DEPS) $(LIB_DEPS_O)
	@$(CCExec_DYNAMIC_O)

test_children-g: $(GEN_DIR)/format7.c test_children.c $(INCLUDE_DEPS) $(LIB_DEPS_D)
	@$(CCExec_DYNAMIC_D)

 # End of rules that are run from the arch directory
endif
