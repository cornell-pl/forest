
 # This is a GNU makefile.

PADS_HOME = ../../..
ifndef AST_ARCH
  AST_ARCH := $(shell $(PADS_HOME)/ast-base/bin/package)
  export AST_ARCH
endif

CURDIR := $(shell pwd)
ifneq ($(AST_ARCH),$(notdir $(CURDIR)))
include $(PADS_HOME)/mk/redirect.mk
else

 # The following rules are run from the arch directory

PADS_HOME = ../../../..
include $(PADS_HOME)/mk/rules.mk

VPATH = ..

HELPERS = myrand cvt_ebcdic_to_ascii cvt_ascii_to_ebcdic dttest

LIBTESTS = rwtest1 \
           recrwtest1 \
           io_openclose_test \
           ebcdic_test \
           cobol_ebc_test \
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

regress : regress_o regress_d

regress_o : sanity_check \
	  regress_o_announce \
          regress_rwtest1_o \
          regress_recrwtest1_o \
          regress_libtest1_o \
          regress_libtest2_o \
          regress_libtest4_o \
          regress_ebcdic_test_o \
          regress_ebcdic_libtest4_o \
          regress_cobol_ebc_test_o \
          regress_bcd_test_o \
          regress_sb_test_o \
          regress_ebc_fpoint_test_o \
          regress_bcd_fpoint_test_o
	@echo " "; echo "DONE"; echo " "

regress_d : sanity_check \
	  regress_d_announce \
          regress_rwtest1_d \
          regress_recrwtest1_d \
          regress_libtest1_d \
          regress_libtest2_d \
          regress_libtest4_d \
          regress_ebcdic_test_d \
          regress_ebcdic_libtest4_d \
          regress_cobol_ebc_test_d \
          regress_bcd_test_d \
          regress_sb_test_d \
          regress_ebc_fpoint_test_d \
          regress_bcd_fpoint_test_d
	@echo " "; echo "DONE"; echo " "

libtests: sanity_check $(LIBTESTS)
accumtests: sanity_check $(ACCUMTESTS)
all: sanity_check $(ALL_PROGS)

sanity_check:
	@$(LibSanityCheck)

regress_o_announce:
	@echo " "; echo "REGRESS USING OPTIMIZED CODE"; echo " " \

regress_d_announce:
	@echo " "; echo "REGRESS USING DEBUG CODE"; echo " " \

regress_rwtest1_o : rwtest1
	@(args="nlrec_noseek"; suf=""; $(RegressDef))
regress_rwtest1_d : rwtest1-g
	@(args="nlrec_noseek"; suf=""; $(RegressDef))

regress_recrwtest1_o : recrwtest1
	@(args="nlrec_noseek"; suf=""; $(RegressDef))
regress_recrwtest1_d : recrwtest1-g
	@(args="nlrec_noseek"; suf=""; $(RegressDef))

regress_libtest1_o : libtest1
	@(args="nlrec_noseek"; suf=""; $(RegressDef))
regress_libtest1_d : libtest1-g
	@(args="nlrec_noseek"; suf=""; $(RegressDef))

regress_libtest2_o : libtest2
	@(args=""; suf=".$(AST_ARCH)"; $(RegressDef))
regress_libtest2_d : libtest2-g
	@(args=""; suf=".$(AST_ARCH)"; $(RegressDef))

regress_libtest4_o : libtest4
	@(args=""; suf=""; $(RegressDef))
regress_libtest4_d : libtest4-g
	@(args=""; suf=""; $(RegressDef))

regress_ebcdic_libtest4_o : ebcdic_libtest4
	@(args=""; suf=""; $(RegressDef))
regress_ebcdic_libtest4_d : ebcdic_libtest4-g
	@(args=""; suf=""; $(RegressDef))

regress_ebcdic_test_o : ebcdic_test
	@(args="ctrec_noseek"; suf=""; $(RegressDef))
regress_ebcdic_test_d : ebcdic_test-g
	@(args="ctrec_noseek"; suf=""; $(RegressDef))

regress_cobol_ebc_test_o : cobol_ebc_test
	@(args="fwrec_noseek"; suf=""; $(RegressDef))
regress_cobol_ebc_test_d : cobol_ebc_test-g
	@(args="fwrec_noseek"; suf=""; $(RegressDef))

regress_bcd_test_o : bcd_test
	@(args="fwrec_noseek"; suf=""; $(RegressDef))
regress_bcd_test_d : bcd_test-g
	@(args="fwrec_noseek"; suf=""; $(RegressDef))

regress_sb_test_o : sb_test
	@(args="fwrec_noseek"; suf=""; $(RegressDef))
regress_sb_test_d : sb_test-g
	@(args="fwrec_noseek"; suf=""; $(RegressDef))

regress_ebc_fpoint_test_o : ebc_fpoint_test
	@(args="fwrec_noseek"; suf=""; $(RegressDef))
regress_ebc_fpoint_test_d : ebc_fpoint_test-g
	@(args="fwrec_noseek"; suf=""; $(RegressDef))

regress_bcd_fpoint_test_o : bcd_fpoint_test
	@(args="fwrec_noseek"; suf=""; $(RegressDef))
regress_bcd_fpoint_test_d : bcd_fpoint_test-g
	@(args="fwrec_noseek"; suf=""; $(RegressDef))

clean:
	$(RM) $(ALL_PROGS) *.o *~ tmp/*

veryclean: clean

 # End of rules that are run from the arch directory
endif


