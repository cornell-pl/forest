
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

# need to set this before including rules.mk
REGRESS_TESTS = 1

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

FORMAT_TESTS = test_format1 test_format2 test_format3 test_format5 test_format6 test_struct_regexp test_array_regexp \
               test_array_nosep test_array_pred test_array_pred2 test_array_ended test_array_ended2 test_array_post \
               test_charclass test_me test_me_fooclass
DIB_TESTS =  test_dibbler1 test_dibbler1_mod test_fast_dib test_fast_dib_mod
WRITE_TESTS = test_write_struct test_write_struct_strings
OTHER_TESTS  = test_array test_array_string test_record_enum test_struct test_struct2 \
               test_punion2 test_typedef \
               test_cpygen_crs test_enum test_from test_ai test_readint test_depends \
               test_pswitch test_pswitch2 test_struct3 manifest_union
ALL_TESTS_O = $(FORMAT_TESTS) $(DIB_TESTS) $(WRITE_TESTS) $(OTHER_TESTS) $(XML_TESTS)
ALL_TESTS_D = $(ALL_TESTS_O:%=%_d)
ALL_TESTS = $(ALL_TESTS_O) $(ALL_TESTS_D)

# do not build MISC tests when building 'all'
MISC_TESTS_O = test_ios 
MISC_TESTS_D = $(MISC_TESTS_O:%=%_d)
MISC_TESTS = $(MISC_TESTS_O) $(MISC_TESTS_D)
CLEANUP_TESTS = $(ALL_TESTS) $(MISC_TESTS)

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
          regress_test_me_fooclass_o \
	  regress_test_write_struct_o \
	  regress_test_write_struct_strings_o \
          regress_test_struct_regexp_o \
          regress_test_array_regexp_o \
          regress_test_array_nosep_o \
          regress_test_array_pred_o  \
          regress_test_array_pred2_o  \
          regress_test_array_ended_o \
          regress_test_array_ended2_o \
          regress_test_array_post_o \
          regress_test_charclass_o

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
          regress_test_me_fooclass_d \
	  regress_test_write_struct_d \
	  regress_test_write_struct_strings_d \
          regress_test_struct_regexp_d \
          regress_test_array_regexp_d \
          regress_test_array_nosep_d \
          regress_test_array_pred_d \
          regress_test_array_pred2_d \
          regress_test_array_ended_d  \
          regress_test_array_ended2_d \
          regress_test_array_post_d \
          regress_test_charclass_d 
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
regress_test_me_d: test_me_d
	@(args=""; suf=""; $(RegressDef))

regress_test_me_fooclass_o: test_me_fooclass
	@(args=""; suf=""; $(RegressDef))
regress_test_me_fooclass_d: test_me_fooclass_d
	@(args=""; suf=""; $(RegressDef))

regress_test_format1_o: test_format1
	@(args=""; input="../../data/ex_data.format1"; suf=""; $(RegressInput))
regress_test_format1_d: test_format1_d
	@(args=""; input="../../data/ex_data.format1"; suf=""; $(RegressInput))

regress_test_format2_o: test_format2
	@(args=""; suf=""; $(RegressDef))
regress_test_format2_d: test_format2_d
	@(args=""; suf=""; $(RegressDef))

regress_test_format3_o: test_format3
	@(args=""; suf=""; $(RegressDef))
regress_test_format3_d: test_format3_d
	@(args=""; suf=""; $(RegressDef))

regress_test_format4_o: test_format4
	@(args=""; suf=""; $(RegressDef))
regress_test_format4_d: test_format4_d
	@(args=""; suf=""; $(RegressDef))

regress_test_format5_o: test_format5
	@(args=""; suf=""; $(RegressDef))
regress_test_format5_d: test_format5_d
	@(args=""; suf=""; $(RegressDef))

regress_test_format6_o: test_format6
	@(args=""; suf=""; $(RegressDef))
regress_test_format6_d: test_format6_d
	@(args=""; suf=""; $(RegressDef))

regress_test_readint_o: test_readint
	@(args=""; suf=""; filter="error:"; $(RegressFilter))
regress_test_readint_d: test_readint_d
	@(args=""; suf=""; filter="error:"; $(RegressFilter))

regress_test_ai_o: test_ai
	@(args=""; input="../../data/ex_data.ai"; suf=""; $(RegressInput))
regress_test_ai_d: test_ai_d
	@(args=""; input="../../data/ex_data.ai"; suf=""; $(RegressInput))

regress_test_write_struct_o: test_write_struct
	@(args=""; suf=""; $(RegressDef))
regress_test_write_struct_d: test_write_struct_d
	@(args=""; suf=""; $(RegressDef))

regress_test_write_struct_strings_o: test_write_struct_strings
	@(args=""; suf=""; $(RegressDef))
regress_test_write_struct_strings_d: test_write_struct_strings_d
	@(args=""; suf=""; $(RegressDef))

regress_test_munion_o: manifest_union
	@(args=""; input="../../data/union_manifest"; suf=""; $(RegressInput))
regress_test_munion_d: manifest_union_d
	@(args=""; input="../../data/union_manifest"; suf=""; $(RegressInput))

regress_test_pswitch_o: test_pswitch
	@(args=""; input="../../data/pswitch"; suf=""; $(RegressInput))
regress_test_pswitch_d: test_pswitch_d
	@(args=""; input="../../data/pswitch"; suf=""; $(RegressInput))

regress_test_pswitch2_o: test_pswitch2
	@(args=""; input="../../data/pswitch"; suf=""; $(RegressInput))
regress_test_pswitch2_d: test_pswitch2_d
	@(args=""; input="../../data/pswitch"; suf=""; $(RegressInput))

regress_test_depends_o: test_depends
	@(args=""; input="../../data/depends"; suf=""; $(RegressInput))
regress_test_depends_d: test_depends_d
	@(args=""; input="../../data/depends"; suf=""; $(RegressInput))

regress_test_struct3_o: test_struct3
	@(args=""; input="../../data/struct3"; suf=""; $(RegressInput))
regress_test_struct3_d: test_struct3_d
	@(args=""; input="../../data/struct3"; suf=""; $(RegressInput))

regress_test_struct_regexp_o: test_struct_regexp
	@(args=""; input="../../data/ex_data.struct_regexp"; suf=""; $(RegressInput))
regress_test_struct_regexp_d: test_struct_regexp_d
	@(args=""; input="../../data/ex_data.struct_regexp"; suf=""; $(RegressInput))

regress_test_array_regexp_o: test_array_regexp
	@(args=""; input="../../data/array_regexp"; suf=""; $(RegressInput))
regress_test_array_regexp_d: test_array_regexp_d
	@(args=""; input="../../data/array_regexp"; suf=""; $(RegressInput))

regress_test_array_nosep_o: test_array_nosep
	@(args=""; input="../../data/array_nosep"; suf=""; $(RegressInput))
regress_test_array_nosep_d: test_array_nosep_d
	@(args=""; input="../../data/array_nosep"; suf=""; $(RegressInput))

regress_test_array_pred_o: test_array_pred
	@(args=""; input="../../data/array_pred"; suf=""; $(RegressInput))
regress_test_array_pred_d: test_array_pred_d
	@(args=""; input="../../data/array_pred"; suf=""; $(RegressInput))

regress_test_array_pred2_o: test_array_pred2
	@(args=""; input="../../data/array_pred2"; suf=""; $(RegressInput))
regress_test_array_pred2_d: test_array_pred2_d
	@(args=""; input="../../data/array_pred2"; suf=""; $(RegressInput))

regress_test_array_ended_o: test_array_ended
	@(args=""; input="../../data/array_ended"; suf=""; $(RegressInput))
regress_test_array_ended_d: test_array_ended_d
	@(args=""; input="../../data/array_ended"; suf=""; $(RegressInput))

regress_test_array_ended2_o: test_array_ended2
	@(args=""; input="../../data/array_ended2"; suf=""; $(RegressInput))
regress_test_array_ended2_d: test_array_ended2_d
	@(args=""; input="../../data/array_ended2"; suf=""; $(RegressInput))

regress_test_array_post_o: test_array_post
	@(args=""; input="../../data/array_post"; suf=""; $(RegressInput))
regress_test_array_post_d: test_array_post_d
	@(args=""; input="../../data/array_post"; suf=""; $(RegressInput))

regress_test_charclass_o: test_charclass
	@(args=""; input="../../data/charclass"; suf=""; $(RegressInput))
regress_test_charclass_d: test_charclass_d
	@(args=""; input="../../data/charclass"; suf=""; $(RegressInput))

regress_test_ios: test_ios
	@(args=""; input="../../data/small_ios_config"; suf=""; pp="$(PADS_HOME)/scripts/ios_pp.pl"; $(RegressInputPP))
regress_test_ios_d: test_ios_d
	@(args=""; input="../../data/small_ios_config"; suf=""; pp="$(PADS_HOME)/scripts/ios_pp.pl"; $(RegressInputPP))

clean:
	$(RM) -f *~ *.o $(CLEANUP_TESTS)

veryclean: clean
	$(RM) $(GEN_DIR)/*.[hc] $(GEN_DIR)/*.xsd tmp/*

# This works for some but not all test cases

# These cases do not fit the usual pattern
test_readint: $(GEN_DIR)/readinttest.c test_readint.c $(INCLUDE_DEPS) $(LIB_DEPS_O)
	@$(CCExec_DYNAMIC_O)
test_readint_d: $(GEN_DIR)/readinttest.c test_readint.c $(INCLUDE_DEPS) $(LIB_DEPS_D)
	@$(CCExec_DYNAMIC_D)

manifest_union: $(GEN_DIR)/punion_manifest.c munion.c $(INCLUDE_DEPS) $(LIB_DEPS_O)
	@$(CCExec_DYNAMIC_O)
manifest_union_d: $(GEN_DIR)/punion_manifest.c munion.c $(INCLUDE_DEPS) $(LIB_DEPS_D)
	@$(CCExec_DYNAMIC_D)

test_copy_format5: $(GEN_DIR)/format5.c test_copy_format5.c $(INCLUDE_DEPS) $(LIB_DEPS_O)
	@$(CCExec_DYNAMIC_O)
test_copy_format5_d: $(GEN_DIR)/format5.c test_copy_format5.c $(INCLUDE_DEPS) $(LIB_DEPS_D)
	@$(CCExec_DYNAMIC_D)

test_write_struct: $(GEN_DIR)/struct.c test_write_struct.c $(INCLUDE_DEPS) $(LIB_DEPS_O)
	@$(CCExec_DYNAMIC_O)
test_write_struct_d: $(GEN_DIR)/struct.c test_write_struct.c $(INCLUDE_DEPS) $(LIB_DEPS_D)
	@$(CCExec_DYNAMIC_D)

test_write_struct_strings: $(GEN_DIR)/struct_strings.c test_write_struct_strings.c $(INCLUDE_DEPS) $(LIB_DEPS_O)
	@$(CCExec_DYNAMIC_O)
test_write_struct_strings_d: $(GEN_DIR)/struct_strings.c test_write_struct_strings.c $(INCLUDE_DEPS) $(LIB_DEPS_D)
	@$(CCExec_DYNAMIC_D)

test_copy_ai: $(GEN_DIR)/ai.c test_copy_ai.c $(INCLUDE_DEPS) $(LIB_DEPS_O)
	@$(CCExec_DYNAMIC_O)
test_copy_ai_d: $(GEN_DIR)/ai.c test_copy_ai.c $(INCLUDE_DEPS) $(LIB_DEPS_D)
	@$(CCExec_DYNAMIC_D)

count_dibbler: $(GEN_DIR)/dibbler_notrailer.c count_dibbler.c $(INCLUDE_DEPS) $(LIB_DEPS_O)
	@$(CCExec_DYNAMIC_O)
count_dibbler_d: $(GEN_DIR)/dibbler_notrailer.c count_dibbler.c $(INCLUDE_DEPS) $(LIB_DEPS_D)
	@$(CCExec_DYNAMIC_D)

test_dibbler1: $(GEN_DIR)/dibbler.c test_dibbler1.c $(INCLUDE_DEDS) $(LIB_DEPS_O)
	@$(CCExec_DYNAMIC_O)
test_dibbler1_d: $(GEN_DIR)/dibbler.c test_dibbler1.c $(INCLUDE_DEDS) $(LIB_DEPS_D)
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
test_children: $(GEN_DIR)/format1.c test_children.c $(INCLUDE_DEPS) $(LIB_DEPS_O)
	@$(CCExec_DYNAMIC_O)

test_children_d: $(GEN_DIR)/format1.c test_children.c $(INCLUDE_DEPS) $(LIB_DEPS_D)
	@$(CCExec_DYNAMIC_D)
endif

 # End of rules that are run from the arch directory
endif
