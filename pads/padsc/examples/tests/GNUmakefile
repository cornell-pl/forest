
 # This is a GNU makefile.  See Makefile for the nmake version.

PADS_HOME = ../..
ifndef AST_ARCH
  AST_ARCH := $(shell $(PADS_HOME)/scripts/package)
  export AST_ARCH
endif

CURDIR := $(shell pwd)
ifneq ($(AST_ARCH),$(notdir $(CURDIR)))
include $(PADS_HOME)/mk/redirect.mk
else

 # The following rules are run from the arch directory

PADS_HOME = ../../..
GEN_DIR = ../../gen
include $(PADS_HOME)/mk/rules.mk

VPATH =  .. ../../p

FORMAT_TESTS = test_format1 test_format2 test_format3 test_format5 test_format6
DIB_TESTS =  test_dibbler1 test_dibbler1_mod test_fast_dib test_fast_dib_mod
OTHER_TESTS  = test_cpygen_crs test_enum test_from test_ai test_readint test_depends test_pswitch test_pswitch2 test_struct3 manifest_union
ALL_TESTS = $(FORMAT_TESTS) $(DIB_TESTS) $(OTHER_TESTS) 

regress:  sanity_check \
          regress_test_format1 \
          regress_test_format2 \
          regress_test_format3 \
          regress_test_format5 \
          regress_test_format6 \
          regress_test_readint \
          regress_test_munion  \
          regress_test_ai \
	  regress_test_pswitch \
	  regress_test_pswitch2 \
	  regress_test_depends \
	  regress_test_struct3
	@echo " "; echo "DONE"; echo " "

dib_tests: sanity_check $(DIB_TESTS)
format_tests: sanity_check $(FORMAT_TESTS)
other_tests: sanity_check $(OTHER_TESTS)
all_tests: sanity_check $(ALL_TESTS)

sanity_check:
	@$(SanityCheck)


regress_test_format1: test_format1
	@(args=""; suf=""; $(RegressDef))

regress_test_format2: test_format2
	@(args=""; suf=""; $(RegressDef))

regress_test_format3: test_format3
	@(args=""; suf=""; $(RegressDef))

regress_test_format4: test_format4
	@(args=""; suf=""; $(RegressDef))

regress_test_format5: test_format5
	@(args=""; suf=""; $(RegressDef))

regress_test_format6: test_format6
	@(args=""; suf=""; $(RegressDef))

regress_test_readint: test_readint
	@(args=""; suf=""; filter="error:"; $(RegressFilter))

regress_test_ai: test_ai
	@(args=""; suf=""; $(RegressDef))

regress_test_munion: manifest_union
	@(args=""; input="../../data/union-manifest"; suf=""; $(RegressInput))

regress_test_pswitch: test_pswitch
	@(args=""; input="../../data/pswitch"; suf=""; $(RegressInput))

regress_test_pswitch2: test_pswitch2
	@(args=""; input="../../data/pswitch"; suf=""; $(RegressInput))

regress_test_depends: test_depends
	@(args=""; input="../../data/depends"; suf=""; $(RegressInput))

regress_test_struct3: test_struct3
	@(args=""; input="../../data/struct3"; suf=""; $(RegressInput))

clean:
	$(RM) -f *~ *.o $(ALL_TESTS)

veryclean: clean
	$(RM) $(GEN_DIR)/*.[hc]

# This works for some but not all test cases

# the following indicates that generated .c files should not be removed
.PRECIOUS: $(GEN_DIR)/%.c 

test_%: $(GEN_DIR)/%.c test_%.c $(INCLUDE_DEPS) $(LIB_DEPS)
	@$(CCExec_DYNAMIC)

# These cases do not fit the usual pattern
test_readint: $(GEN_DIR)/readinttest.c test_readint.c $(INCLUDE_DEPS) $(LIB_DEPS)
	@$(CCExec_DYNAMIC)

manifest_union: $(GEN_DIR)/punion-manifest.c munion.c $(INCLUDE_DEPS) $(LIB_DEPS)
	@$(CCExec_DYNAMIC)

test_copy_format5: $(GEN_DIR)/format5.c test_copy_format5.c $(INCLUDE_DEPS) $(LIB_DEPS)
	@$(CCExec_DYNAMIC)

 # End of rules that are run from the arch directory
endif

