
 # This is an nmake file.  See GNUmakefile for the gmake version.

if ! "$(PWD:N=*/$(CC.HOSTTYPE))"

PADS_HOME = ../..
include $(PADS_HOME)/mk/redirect.nmake

else

 # The following rules are run from the arch directory

:PACKAGE: ast

PADS_HOME = ../../..
include $(PADS_HOME)/mk/rules.nmake

.SOURCE : .. $(PADS_HOME)/include

SCRIPTS = $(PADS_HOME)/scripts
REGRESS_DIR = ../../regress

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

regress : regress_libtest1 \
          regress_libtest2 \
          regress_libtest4 \
          regress_ebcdic_test \
          regress_ebcdic_libtest4 \
          regress_ebc_test \
          regress_bcd_test \
          regress_sb_test \
          regress_ebc_fpoint_test \
          regress_bcd_fpoint_test
	(set +x; echo " "; echo "DONE"; echo " ")

libtests: $(LIBTESTS)
accumtests: $(ACCUMTESTS)
all: $(ALL_PROGS)

myrand :: myrand.c
cvt_ebcdic_to_ascii :: cvt_ebcdic_to_ascii.c
cvt_ascii_to_ebcdic :: cvt_ascii_to_ebcdic.c

adate_test       :: adate_test.c        -lpadsc
libtest1         :: libtest1.c          -lpadsc
biglibtest1      :: biglibtest1.c       -lpadsc
libtest2         :: libtest2.c          -lpadsc
libtest4         :: libtest4.c          -lpadsc
ebcdic_libtest4  :: ebcdic_libtest4.c   -lpadsc
rbuftest1        :: rbuftest1.c         -lpadsc
ebcdic_test      :: ebcdic_test.c       -lpadsc
ebc_test         :: ebc_test.c          -lpadsc
bcd_test         :: bcd_test.c          -lpadsc
sb_test          :: sb_test.c           -lpadsc
ebc_fpoint_test  :: ebc_fpoint_test.c   -lpadsc
bcd_fpoint_test  :: bcd_fpoint_test.c   -lpadsc

dttest         :: dttest.c        -lpadsc
accumtest      :: accumtest.c     -lpadsc
accumtest2     :: accumtest2.c    -lpadsc
accumtest_s    :: accumtest_s.c   -lpadsc

string_share_test   :: string_share_test.c          -lpadsc

mk_ex_data_biglibtest1 :: mk_ex_data_biglibtest1.c        -lpadsc
mk_ex_data_libtest2    :: mk_ex_data_libtest2.c           -lpadsc
mk_ex_data_ebcdic_test :: mk_ex_data_ebcdic_test.c        -lpadsc
mk_ex_data_ebc_test    :: mk_ex_data_ebc_test.c           -lpadsc
mk_ex_data_bcd_test    :: mk_ex_data_bcd_test.c           -lpadsc
mk_ex_data_sb_test     :: mk_ex_data_sb_test.c            -lpadsc

regress : regress_libtest1 \
          regress_libtest2 \
          regress_libtest4 \
          regress_ebcdic_test \
          regress_ebcdic_libtest4 \
          regress_ebc_test \
          regress_bcd_test \
          regress_sb_test \
          regress_ebc_fpoint_test \
          regress_bcd_fpoint_test \

regress_libtest1 : libtest1
	(set +x; if [ -f tmp/tmp.libtest1 ]; then $(RM) tmp/tmp.libtest1; fi)
	(set +x; echo "(./libtest1 nlrec_noseek 2>&1) | $(SCRIPTS)/remove_junk.pl | cat > tmp/tmp.libtest1")
	(set +x; ((./libtest1 nlrec_noseek 2>&1) | $(SCRIPTS)/remove_junk.pl | cat > tmp/tmp.libtest1))
	diff tmp/tmp.libtest1 $(REGRESS_DIR)/libtest1.regress || echo LIBTEST1 DIFFERS
	(set +x; echo " ")

regress_libtest2 : libtest2
	(set +x; if [ -f tmp/tmp.libtest2.$(CC.HOSTTYPE) ]; then $(RM) tmp/tmp.libtest2.$(CC.HOSTTYPE); fi)
	(set +x; echo "(./libtest2 2>&1) | $(SCRIPTS)/remove_junk.pl | cat > tmp/tmp.libtest2.$(CC.HOSTTYPE)")
	(set +x; (./libtest2 2>&1) | $(SCRIPTS)/remove_junk.pl | cat > tmp/tmp.libtest2.$(CC.HOSTTYPE))
	diff tmp/tmp.libtest2.$(CC.HOSTTYPE) $(REGRESS_DIR)/libtest2.regress.$(CC.HOSTTYPE) || echo LIBTEST2 DIFFERS
	(set +x; echo " ")

regress_libtest4 : libtest4
	(set +x; if [ -f tmp/tmp.libtest4 ]; then $(RM) tmp/tmp.libtest4; fi)
	(set +x; echo "(./libtest4 2>&1) | $(SCRIPTS)/remove_junk.pl | cat > tmp/tmp.libtest4")
	(set +x; (./libtest4 2>&1) | $(SCRIPTS)/remove_junk.pl | cat > tmp/tmp.libtest4)
	diff tmp/tmp.libtest4 $(REGRESS_DIR)/libtest4.regress || echo LIBTEST4 DIFFERS
	(set +x; echo " ")

regress_ebcdic_libtest4 : ebcdic_libtest4
	(set +x; if [ -f tmp/tmp.ebcdic_libtest4 ]; then $(RM) tmp/tmp.ebcdic_libtest4; fi)
	(set +x; echo "(./ebcdic_libtest4 2>&1) | $(SCRIPTS)/remove_junk.pl | cat > tmp/tmp.ebcdic_libtest4")
	(set +x; (./ebcdic_libtest4 2>&1) | $(SCRIPTS)/remove_junk.pl | cat > tmp/tmp.ebcdic_libtest4)
	diff tmp/tmp.ebcdic_libtest4 $(REGRESS_DIR)/ebcdic_libtest4.regress || echo EBCDIC_LIBTEST4 DIFFERS
	(set +x; echo " ")

regress_ebcdic_test : ebcdic_test
	(set +x; if [ -f tmp/tmp.ebcdic_test ]; then $(RM) tmp/tmp.ebcdic_test; fi)
	(set +x; echo "(./ebcdic_test ctrec_noseek 2>&1) | $(SCRIPTS)/remove_junk.pl | cat > tmp/tmp.ebcdic_test")
	(set +x; (./ebcdic_test ctrec_noseek 2>&1) | $(SCRIPTS)/remove_junk.pl | cat > tmp/tmp.ebcdic_test)
	diff tmp/tmp.ebcdic_test $(REGRESS_DIR)/ebcdic_test.regress || echo EBCDIC_TEST DIFFERS
	(set +x; echo " ")

regress_ebc_test : ebc_test
	(set +x; if [ -f tmp/tmp.ebc_test ]; then $(RM) tmp/tmp.ebc_test; fi)
	(set +x; echo "(./ebc_test fwrec_noseek 2>&1) | $(SCRIPTS)/remove_junk.pl | cat > tmp/tmp.ebc_test")
	(set +x; (./ebc_test fwrec_noseek 2>&1) | $(SCRIPTS)/remove_junk.pl | cat > tmp/tmp.ebc_test)
	diff tmp/tmp.ebc_test $(REGRESS_DIR)/ebc_test.regress || echo EBC_TEST DIFFERS
	(set +x; echo " ")

regress_bcd_test : bcd_test
	(set +x; if [ -f tmp/tmp.bcd_test ]; then $(RM) tmp/tmp.bcd_test; fi)
	(set +x; echo "(./bcd_test fwrec_noseek 2>&1) | $(SCRIPTS)/remove_junk.pl | cat > tmp/tmp.bcd_test")
	(set +x; (./bcd_test fwrec_noseek 2>&1) | $(SCRIPTS)/remove_junk.pl | cat > tmp/tmp.bcd_test)
	diff tmp/tmp.bcd_test $(REGRESS_DIR)/bcd_test.regress || echo BCD_TEST DIFFERS
	(set +x; echo " ")

regress_sb_test : sb_test
	(set +x; if [ -f tmp/tmp.sb_test ]; then $(RM) tmp/tmp.sb_test; fi)
	(set +x; echo "(./sb_test fwrec_noseek 2>&1) | $(SCRIPTS)/remove_junk.pl | cat > tmp/tmp.sb_test")
	(set +x; (./sb_test fwrec_noseek 2>&1) | $(SCRIPTS)/remove_junk.pl | cat > tmp/tmp.sb_test)
	diff tmp/tmp.sb_test $(REGRESS_DIR)/sb_test.regress || echo SB_TEST DIFFERS
	(set +x; echo " ")

regress_ebc_fpoint_test : ebc_fpoint_test
	(set +x; if [ -f tmp/tmp.ebc_fpoint_test ]; then $(RM) tmp/tmp.ebc_fpoint_test; fi)
	(set +x; echo "(./ebc_fpoint_test fwrec_noseek 2>&1) | $(SCRIPTS)/remove_junk.pl | cat > tmp/tmp.ebc_fpoint_test")
	(set +x; (./ebc_fpoint_test fwrec_noseek 2>&1) | $(SCRIPTS)/remove_junk.pl | cat > tmp/tmp.ebc_fpoint_test)
	diff tmp/tmp.ebc_fpoint_test $(REGRESS_DIR)/ebc_fpoint_test.regress || echo EBC_FPOINT_TEST DIFFERS
	(set +x; echo " ")

regress_bcd_fpoint_test : bcd_fpoint_test
	(set +x; if [ -f tmp/tmp.bcd_fpoint_test ]; then $(RM) tmp/tmp.bcd_fpoint_test; fi)
	(set +x; echo "(./bcd_fpoint_test fwrec_noseek 2>&1) | $(SCRIPTS)/remove_junk.pl | cat > tmp/tmp.bcd_fpoint_test")
	(set +x; (./bcd_fpoint_test fwrec_noseek 2>&1) | $(SCRIPTS)/remove_junk.pl | cat > tmp/tmp.bcd_fpoint_test)
	diff tmp/tmp.bcd_fpoint_test $(REGRESS_DIR)/bcd_fpoint_test.regress || echo BCD_FPOINT_TEST DIFFERS
	(set +x; echo " ")

clean :
	$(RM) -f $(ALL_PROGS) *.o *~ ../*~

veryclean : clean
	$(RM) -f Makefile.ms Makefile.mo ../Makefile.ms ../Makefile.mo tmp/*

 # End of rules are run from the arch directory
end
