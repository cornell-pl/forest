
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
include $(PADS_HOME)/mk/rules.mk

VPATH = ..

HELPERS = myrand cvt_ebcdic_to_ascii cvt_ascii_to_ebcdic dttest

LIBTESTS = ebcdic_test \
           ebc_test \
           bcd_test \
           sb_test \
           ebc_fpoint_test \
           bcd_fpoint_test \
           adate_test \
           mk_ex_data_libtest2 \
           mk_ex_data_ebcdic_test \
           mk_ex_data_ebc_test \
           mk_ex_data_bcd_test \
           mk_ex_data_sb_test \
           libtest1 \
           libtest2 \
           ebcdic_libtest4 \
           libtest4 \
           rbuftest1 \
           mk_ex_data_biglibtest1 \
           biglibtest1 \
           string_share_test

ACCUMTESTS = accumtest accumtest2 accumtest_s

ALL_PROGS = $(HELPERS) $(LIBTESTS) $(ACCUMTESTS)

regress : sanity_check \
          regress_libtest1 \
          regress_libtest2 \
          regress_libtest4 \
          regress_ebcdic_test \
          regress_ebcdic_libtest4 \
          regress_ebc_test \
          regress_bcd_test \
          regress_sb_test \
          regress_ebc_fpoint_test \
          regress_bcd_fpoint_test
	@echo " "; echo "DONE"; echo " "

libtests: sanity_check $(LIBTESTS)
accumtests: sanity_check $(ACCUMTESTS)
all: sanity_check $(ALL_PROGS)

sanity_check:
	@$(SanityCheck)

regress_libtest1 : libtest1
	@(args="nlrec_noseek"; suf=""; $(RegressDef))

regress_libtest2 : libtest2
	@(args=""; suf=".$(AST_ARCH)"; $(RegressDef))

regress_libtest4 : libtest4
	@(args=""; suf=""; $(RegressDef))

regress_ebcdic_libtest4 : ebcdic_libtest4
	@(args=""; suf=""; $(RegressDef))

regress_ebcdic_test : ebcdic_test
	@(args="ctrec_noseek"; suf=""; $(RegressDef))

regress_ebc_test : ebc_test
	@(args="fwrec_noseek"; suf=""; $(RegressDef))

regress_bcd_test : bcd_test
	@(args="fwrec_noseek"; suf=""; $(RegressDef))

regress_sb_test : sb_test
	@(args="fwrec_noseek"; suf=""; $(RegressDef))

regress_ebc_fpoint_test : ebc_fpoint_test
	@(args="fwrec_noseek"; suf=""; $(RegressDef))

regress_bcd_fpoint_test : bcd_fpoint_test
	@(args="fwrec_noseek"; suf=""; $(RegressDef))

clean:
	$(RM) $(ALL_PROGS) *.o *~ tmp/*

veryclean: clean

 # End of rules that are run from the arch directory
endif


