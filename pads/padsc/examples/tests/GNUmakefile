
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

# the following indicates that generated .c files should not be removed
.PRECIOUS: $(GEN_DIR)/%.c 

# need to give these rules before including rules.mk

test_%-g: $(GEN_DIR)/%.c test_%.c $(INCLUDE_DEPS) $(LIB_DEPS_D)
	@$(CCExec_DYNAMIC_D)

test_%: $(GEN_DIR)/%.c test_%.c $(INCLUDE_DEPS) $(LIB_DEPS_O)
	@$(CCExec_DYNAMIC_O)

include $(PADS_HOME)/mk/rules.mk

VPATH =  .. ../../p

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

FORMAT_TESTS = test_format1 test_format2 test_format3 test_format5 test_format6
DIB_TESTS =  test_dibbler1 test_dibbler1_mod test_fast_dib test_fast_dib_mod
WRITE_TESTS = test_write_struct test_write_struct_strings
OTHER_TESTS  = test_cpygen_crs test_enum test_from test_ai test_readint test_depends test_pswitch test_pswitch2 test_struct3 manifest_union
ALL_TESTS_O = $(FORMAT_TESTS) $(DIB_TESTS) $(WRITE_TESTS) $(OTHER_TESTS) $(XML_TESTS)
ALL_TESTS_D = $(ALL_TESTS_O:%=%-g)
ALL_TESTS = $(ALL_TESTS_O) $(ALL_TESTS_D)

regress: regress_o regress_d

regress_o:  sanity_check \
	  regress_o_announce \
          regress_test_format1_o \
          regress_test_format2_o \
          regress_test_format3_o \
          regress_test_format5_o \
          regress_test_format6_o \
          regress_test_readint_o \
          regress_test_munion_o \
          regress_test_ai_o \
	  regress_test_pswitch_o \
	  regress_test_pswitch2_o \
	  regress_test_depends_o \
	  regress_test_struct3_o \
          regress_test_me_o \
	  regress_test_write_struct_o \
	  regress_test_write_struct_strings_o
	@echo " "; echo "DONE"; echo " "

regress_d:  sanity_check \
	  regress_d_announce \
          regress_test_format1_d \
          regress_test_format2_d \
          regress_test_format3_d \
          regress_test_format5_d \
          regress_test_format6_d \
          regress_test_readint_d \
          regress_test_munion_d \
          regress_test_ai_d \
	  regress_test_pswitch_d \
	  regress_test_pswitch2_d \
	  regress_test_depends_d \
	  regress_test_struct3_d \
          regress_test_me_d \
	  regress_test_write_struct_d \
	  regress_test_write_struct_strings_d
	@echo " "; echo "DONE"; echo " "

dib_tests: sanity_check $(DIB_TESTS)
format_tests: sanity_check $(FORMAT_TESTS)
other_tests: sanity_check $(OTHER_TESTS)
all_tests: sanity_check $(ALL_TESTS)

sanity_check:
	@$(SanityCheck)

regress_o_announce:
	@echo " "; echo "REGRESS USING OPTIMIZED CODE"; echo " " \

regress_d_announce:
	@echo " "; echo "REGRESS USING DEBUG CODE"; echo " " \


regress_test_me_o: test_me
	@(args=""; suf=""; $(RegressDef))
regress_test_me_d: test_me-g
	@(args=""; suf=""; $(RegressDef))

regress_test_format1_o: test_format1
	@(args=""; suf=""; $(RegressDef))
regress_test_format1_d: test_format1-g
	@(args=""; suf=""; $(RegressDef))

regress_test_format2_o: test_format2
	@(args=""; suf=""; $(RegressDef))
regress_test_format2_d: test_format2-g
	@(args=""; suf=""; $(RegressDef))

regress_test_format3_o: test_format3
	@(args=""; suf=""; $(RegressDef))
regress_test_format3_d: test_format3-g
	@(args=""; suf=""; $(RegressDef))

regress_test_format4_o: test_format4
	@(args=""; suf=""; $(RegressDef))
regress_test_format4_d: test_format4-g
	@(args=""; suf=""; $(RegressDef))

regress_test_format5_o: test_format5
	@(args=""; suf=""; $(RegressDef))
regress_test_format5_d: test_format5-g
	@(args=""; suf=""; $(RegressDef))

regress_test_format6_o: test_format6
	@(args=""; suf=""; $(RegressDef))
regress_test_format6_d: test_format6-g
	@(args=""; suf=""; $(RegressDef))

regress_test_readint_o: test_readint
	@(args=""; suf=""; filter="error:"; $(RegressFilter))
regress_test_readint_d: test_readint-g
	@(args=""; suf=""; filter="error:"; $(RegressFilter))

regress_test_ai_o: test_ai
	@(args=""; suf=""; $(RegressDef))
regress_test_ai_d: test_ai-g
	@(args=""; suf=""; $(RegressDef))

regress_test_write_struct_o: test_write_struct
	@(args=""; suf=""; $(RegressDef))
regress_test_write_struct_d: test_write_struct-g
	@(args=""; suf=""; $(RegressDef))

regress_test_write_struct_strings_o: test_write_struct_strings
	@(args=""; suf=""; $(RegressDef))
regress_test_write_struct_strings_d: test_write_struct_strings-g
	@(args=""; suf=""; $(RegressDef))

regress_test_munion_o: manifest_union
	@(args=""; input="../../data/union-manifest"; suf=""; $(RegressInput))
regress_test_munion_d: manifest_union-g
	@(args=""; input="../../data/union-manifest"; suf=""; $(RegressInput))

regress_test_pswitch_o: test_pswitch
	@(args=""; input="../../data/pswitch"; suf=""; $(RegressInput))
regress_test_pswitch_d: test_pswitch-g
	@(args=""; input="../../data/pswitch"; suf=""; $(RegressInput))

regress_test_pswitch2_o: test_pswitch2
	@(args=""; input="../../data/pswitch"; suf=""; $(RegressInput))
regress_test_pswitch2_d: test_pswitch2-g
	@(args=""; input="../../data/pswitch"; suf=""; $(RegressInput))

regress_test_depends_o: test_depends
	@(args=""; input="../../data/depends"; suf=""; $(RegressInput))
regress_test_depends_d: test_depends-g
	@(args=""; input="../../data/depends"; suf=""; $(RegressInput))

regress_test_struct3_o: test_struct3
	@(args=""; input="../../data/struct3"; suf=""; $(RegressInput))
regress_test_struct3_d: test_struct3-g
	@(args=""; input="../../data/struct3"; suf=""; $(RegressInput))

clean:
	$(RM) -f *~ *.o $(ALL_TESTS)

veryclean: clean
	$(RM) $(GEN_DIR)/*.[hc]

# This works for some but not all test cases

# These cases do not fit the usual pattern
test_readint: $(GEN_DIR)/readinttest.c test_readint.c $(INCLUDE_DEPS) $(LIB_DEPS_O)
	@$(CCExec_DYNAMIC_O)
test_readint-g: $(GEN_DIR)/readinttest.c test_readint.c $(INCLUDE_DEPS) $(LIB_DEPS_D)
	@$(CCExec_DYNAMIC_D)

manifest_union: $(GEN_DIR)/punion-manifest.c munion.c $(INCLUDE_DEPS) $(LIB_DEPS_O)
	@$(CCExec_DYNAMIC_O)
manifest_union-g: $(GEN_DIR)/punion-manifest.c munion.c $(INCLUDE_DEPS) $(LIB_DEPS_D)
	@$(CCExec_DYNAMIC_D)

test_copy_format5: $(GEN_DIR)/format5.c test_copy_format5.c $(INCLUDE_DEPS) $(LIB_DEPS_O)
	@$(CCExec_DYNAMIC_O)
test_copy_format5-g: $(GEN_DIR)/format5.c test_copy_format5.c $(INCLUDE_DEPS) $(LIB_DEPS_D)
	@$(CCExec_DYNAMIC_D)

test_write_struct: $(GEN_DIR)/struct.c test_write_struct.c $(INCLUDE_DEPS) $(LIB_DEPS_O)
	@$(CCExec_DYNAMIC_O)
test_write_struct-g: $(GEN_DIR)/struct.c test_write_struct.c $(INCLUDE_DEPS) $(LIB_DEPS_D)
	@$(CCExec_DYNAMIC_D)

test_write_struct_strings: $(GEN_DIR)/struct_strings.c test_write_struct_strings.c $(INCLUDE_DEPS) $(LIB_DEPS_O)
	@$(CCExec_DYNAMIC_O)
test_write_struct_strings-g: $(GEN_DIR)/struct_strings.c test_write_struct_strings.c $(INCLUDE_DEPS) $(LIB_DEPS_D)
	@$(CCExec_DYNAMIC_D)

test_copy_ai: $(GEN_DIR)/ai.c test_copy_ai.c $(INCLUDE_DEPS) $(LIB_DEPS_O)
	@$(CCExec_DYNAMIC_O)
test_copy_ai-g: $(GEN_DIR)/ai.c test_copy_ai.c $(INCLUDE_DEPS) $(LIB_DEPS_D)
	@$(CCExec_DYNAMIC_D)

count_dibbler: $(GEN_DIR)/dibbler-notrailer.c count_dibbler.c $(INCLUDE_DEPS) $(LIB_DEPS_O)
	@$(CCExec_DYNAMIC_O)
count_dibbler-g: $(GEN_DIR)/dibbler-notrailer.c count_dibbler.c $(INCLUDE_DEPS) $(LIB_DEPS_D)
	@$(CCExec_DYNAMIC_D)

test_dibbler1: $(GEN_DIR)/dibbler.c test_dibbler1.c $(INCLUDE_DEDS) $(LIB_DEPS_O)
	@$(CCExec_DYNAMIC_O)
test_dibbler1-g: $(GEN_DIR)/dibbler.c test_dibbler1.c $(INCLUDE_DEDS) $(LIB_DEPS_D)
	@$(CCExec_DYNAMIC_D)

ifdef GALAX_NONE
test_children: var_error

test_children-g: var_error

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
test_children: $(GEN_DIR)/format1.c test_children.c $(INCLUDE_DEPS) $(LIB_DEPS_O)
	@$(CCExec_DYNAMIC_O)

test_children-g: $(GEN_DIR)/format1.c test_children.c $(INCLUDE_DEPS) $(LIB_DEPS_D)
	@$(CCExec_DYNAMIC_D)
endif

 # End of rules that are run from the arch directory
endif
