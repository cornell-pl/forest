
 # This is an nmake file.  See GNUmakefile for the gmake version.

if ! "$(PWD:N=*/$(CC.HOSTTYPE))"

PADS_HOME = ../..
include $(PADS_HOME)/mk/redirect.nmake

else

 # The following rules are run from the arch directory

:PACKAGE: ast

PADS_HOME = ../../..
P_DIR = ../../p
GEN_DIR = ../../gen
REGRESS_DIR = ../../regress
DATA_DIR = ../../data

.SOURCE : .. $(PADS_HOME)/include $(GEN_DIR) $(P_DIR)

if 0
CCFLAGS = $(CC.OPTIMIZE) -DNDEBUG
end
CCFLAGS = $(CC.DEBUG)


RES_SPEC = -r $(GEN_DIR)
SCRIPTS = $(PADS_HOME)/scripts
PADSC = $(PADS_HOME)/padsc
ARCH_N_OPSYS = $(sh $(SCRIPTS)/arch-n-opsys)
PADSC_REAL = $(PADS_HOME)/lib/padsc.$(ARCH_N_OPSYS)

PADS_TESTS = format1 format2 format3 format4 format5 format6 punion-manifest \
             readinttest ai pswitch pswitch2 depends
FORMAT_TESTS = test_format1 test_format2 test_format3 test_format5 test_format6
DIB_TESTS =  test_dibbler1 test_dibbler1_mod test_fast_dib test_fast_dib_mod
OTHER_TESTS  = test_enum test_from test_ai test_readint test_depends
ALL_TESTS = $(FORMAT_TESTS) $(DIB_TESTS) $(OTHER_TESTS) 

regress : regress_test_format1 \
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
	(set +x; echo " "; echo "DONE"; echo " ")

dib_tests : $(DIB_TESTS)

format_tests : $(FORMAT_TESTS)

other_tests : $(OTHER_TESTS)

all_tests : $(ALL_TESTS)

test_select_billing_tn  :: test_select_billing_tn.c   dibbler.c     -lpadsc

test_format1  :: test_format1.c   format1.c     -lpadsc
test_format2  :: test_format2.c   format2.c     -lpadsc
test_format3  :: test_format3.c   format3.c     -lpadsc
test_format4  :: test_format4.c   format4.c     -lpadsc
test_format5  :: test_format5.c   format5.c     -lpadsc
test_format6  :: test_format6.c   format6.c     -lpadsc

accum_struct  :: testtwo.c        struct.c      -lpadsc
accum_ai      :: http_clf_t.c     ai.c          -lpadsc

test_pswitch3 :: test_pswitch3.c  pswitch3.c     -lpadsc
test_pswitch2 :: test_pswitch2.c  pswitch2.c     -lpadsc
test_pswitch  :: test_pswitch.c   pswitch.c     -lpadsc
manifest_union :: munion.c        punion-manifest.c -lpadsc
test_struct   :: test_struct.c    struct.c      -lpadsc
test_struct2  :: test_struct2.c   struct2.c     -lpadsc
test_struct3  :: test_struct3.c   struct3.c     -lpadsc
test_union2   :: test_punion2.c   punion2.c     -lpadsc
test_array    :: test_array.c     array.c       -lpadsc
test_typedef  :: test_typedef.c   typedef.c     -lpadsc
test_record_enum :: test_record_enum.c record_enum.c -lpadsc
test_enum     :: test_enum.c      enum.c        -lpadsc
test_from     :: test_from.c      from.c        -lpadsc
test_ai       :: test_ai.c        ai.c          -lpadsc
test_readint  :: test_readint.c   readinttest.c -lpadsc
test_dibbler1 :: test_dibbler1.c  dibbler.c     -lpadsc
test_dibbler1_mod :: test_dibbler1_mod.c  dibbler-mod.c     -lpadsc
test_fast_dib :: test_fast_dib.c  dibbler2.c    -lpadsc
test_fast_dib_mod :: test_fast_dib_mod.c  dibbler2-mod.c    -lpadsc
test_depends  :: test_depends.c   depends.c     -lpadsc
test_copy     :: test_copy.c	  format1.c	-lpadsc
test_copy_format3     :: test_copy_format3.c	  format3.c	-lpadsc
test_copy_format5     :: test_copy_format5.c	  format5.c	-lpadsc
test_write_struct     :: test_write_struct.c      struct.c      -lpadsc

regress_test_format1 : test_format1
	(set +x; test -d tmp || mkdir tmp)
	(set +x; if [ -f tmp/tmp.test_format1 ]; then $(RM) tmp/tmp.test_format1; fi)
	(set +x; echo "(./test_format1 2>&1) | $(PADS_HOME)/scripts/remove_junk.pl | cat > tmp/tmp.test_format1")
	(set +x; (./test_format1 2>&1) | $(PADS_HOME)/scripts/remove_junk.pl | cat > tmp/tmp.test_format1)
	diff tmp/tmp.test_format1 $(REGRESS_DIR)/test_format1.regress || echo TEST_FORMAT1 DIFFERS
	(set +x; echo " ")

regress_test_format2 : test_format2
	(set +x; test -d tmp || mkdir tmp)
	(set +x; if [ -f tmp/tmp.test_format2 ]; then $(RM) tmp/tmp.test_format2; fi)
	(set +x; echo "(./test_format2 2>&1) | $(PADS_HOME)/scripts/remove_junk.pl | cat > tmp/tmp.test_format2")
	(set +x; (./test_format2 2>&1) | $(PADS_HOME)/scripts/remove_junk.pl | cat > tmp/tmp.test_format2)
	diff tmp/tmp.test_format2 $(REGRESS_DIR)/test_format2.regress || echo TEST_FORMAT2 DIFFERS
	(set +x; echo " ")

regress_test_format3 : test_format3
	(set +x; test -d tmp || mkdir tmp)
	(set +x; if [ -f tmp/tmp.test_format3 ]; then $(RM) tmp/tmp.test_format3; fi)
	(set +x; echo "(./test_format3 2>&1) | $(PADS_HOME)/scripts/remove_junk.pl | cat > tmp/tmp.test_format3")
	(set +x; (./test_format3 2>&1) | $(PADS_HOME)/scripts/remove_junk.pl | cat > tmp/tmp.test_format3)
	diff tmp/tmp.test_format3 $(REGRESS_DIR)/test_format3.regress || echo TEST_FORMAT3 DIFFERS
	(set +x; echo " ")

regress_test_format4 : test_format4
	(set +x; test -d tmp || mkdir tmp)
	(set +x; if [ -f tmp/tmp.test_format4 ]; then $(RM) tmp/tmp.test_format4; fi)
	(set +x; echo "(./test_format4 2>&1) | $(PADS_HOME)/scripts/remove_junk.pl | cat > tmp/tmp.test_format4")
	(set +x; (./test_format4 2>&1) | $(PADS_HOME)/scripts/remove_junk.pl | cat > tmp/tmp.test_format4)
	diff tmp/tmp.test_format4 $(REGRESS_DIR)/test_format4.regress || echo TEST_FORMAT4 DIFFERS
	(set +x; echo " ")

regress_test_format5 : test_format5
	(set +x; test -d tmp || mkdir tmp)
	(set +x; if [ -f tmp/tmp.test_format5 ]; then $(RM) tmp/tmp.test_format5; fi)
	(set +x; echo "(./test_format5 2>&1) | $(PADS_HOME)/scripts/remove_junk.pl | cat > tmp/tmp.test_format5")
	(set +x; (./test_format5 2>&1) | $(PADS_HOME)/scripts/remove_junk.pl | cat > tmp/tmp.test_format5)
	diff tmp/tmp.test_format5 $(REGRESS_DIR)/test_format5.regress || echo TEST_FORMAT5 DIFFERS
	(set +x; echo " ")

regress_test_format6 : test_format6
	(set +x; test -d tmp || mkdir tmp)
	(set +x; if [ -f tmp/tmp.test_format6 ]; then $(RM) tmp/tmp.test_format6; fi)
	(set +x; echo "(./test_format6 2>&1) | $(PADS_HOME)/scripts/remove_junk.pl | cat > tmp/tmp.test_format6")
	(set +x; (./test_format6 2>&1) | $(PADS_HOME)/scripts/remove_junk.pl | cat > tmp/tmp.test_format6)
	diff tmp/tmp.test_format6 $(REGRESS_DIR)/test_format6.regress || echo TEST_FORMAT6 DIFFERS
	(set +x; echo " ")

regress_test_readint : test_readint
	(set +x; test -d tmp || mkdir tmp)
	(set +x; if [ -f tmp/tmp.test_readint ]; then $(RM) tmp/tmp.test_readint; fi)
	(set +x; echo "(./test_readint 2>&1) | $(PADS_HOME)/scripts/remove_junk.pl | grep error: | cat > tmp/tmp.test_readint")
	(set +x; (./test_readint 2>&1) | $(PADS_HOME)/scripts/remove_junk.pl | grep error: | cat > tmp/tmp.test_readint)
	diff tmp/tmp.test_readint $(REGRESS_DIR)/test_readint.regress || echo TEST_READINT DIFFERS
	(set +x; echo " ")

regress_test_munion : manifest_union
	(set +x; test -d tmp || mkdir tmp)
	(set +x; if [ -f tmp/tmp.manifest_union ]; then $(RM) tmp/tmp.manifest_union; fi)
	(set +x; echo "(./manifest_union < $(DATA_DIR)/union-manifest 2>&1) | $(PADS_HOME)/scripts/remove_junk.pl | cat > tmp/tmp.manifest_union")
	(set +x; (./manifest_union <$(DATA_DIR)/union-manifest 2>&1) | $(PADS_HOME)/scripts/remove_junk.pl | cat > tmp/tmp.manifest_union)
	diff tmp/tmp.manifest_union $(REGRESS_DIR)/manifest_union.regress || echo TEST_MUNION DIFFERS
	(set +x; echo " ")

regress_test_ai : test_ai
	(set +x; test -d tmp || mkdir tmp)
	(set +x; if [ -f tmp/tmp.test_ai ]; then $(RM) tmp/tmp.test_ai; fi)
	(set +x; echo "(./test_ai 2>&1) | $(PADS_HOME)/scripts/remove_junk.pl | cat > tmp/tmp.test_ai")
	(set +x; (./test_ai 2>&1) | $(PADS_HOME)/scripts/remove_junk.pl | cat > tmp/tmp.test_ai)
	diff tmp/tmp.test_ai $(REGRESS_DIR)/test_ai.regress || echo TEST_AI DIFFERS
	(set +x; echo " ")

regress_test_pswitch : test_pswitch
	(set +x; test -d tmp || mkdir tmp)
	(set +x; if [ -f tmp/tmp.pswitch ]; then $(RM) tmp/tmp.pswitch; fi)
	(set +x; echo "(./test_pswitch <$(DATA_DIR)/pswitch 2>&1) | $(PADS_HOME)/scripts/remove_junk.pl | cat > tmp/tmp.pswitch")
	(set +x; (./test_pswitch <$(DATA_DIR)/pswitch 2>&1) | $(PADS_HOME)/scripts/remove_junk.pl | cat > tmp/tmp.pswitch)
	diff tmp/tmp.pswitch $(REGRESS_DIR)/test_pswitch.regress || echo TEST_PSWITCH DIFFERS
	(set +x; echo " ")

regress_test_pswitch2 : test_pswitch2
	(set +x; test -d tmp || mkdir tmp)
	(set +x; if [ -f tmp/tmp.pswitch2 ]; then $(RM) tmp/tmp.pswitch2; fi)
	(set +x; echo "(./test_pswitch2 <$(DATA_DIR)/pswitch 2>&1) | $(PADS_HOME)/scripts/remove_junk.pl | cat > tmp/tmp.pswitch2")
	(set +x; (./test_pswitch2 <$(DATA_DIR)/pswitch 2>&1) | $(PADS_HOME)/scripts/remove_junk.pl | cat > tmp/tmp.pswitch2)
	diff tmp/tmp.pswitch2 $(REGRESS_DIR)/test_pswitch2.regress || echo TEST_PSWITCH2 DIFFERS
	(set +x; echo " ")

regress_test_depends : test_depends
	(set +x; test -d tmp || mkdir tmp)
	(set +x; if [ -f tmp/tmp.depends ]; then $(RM) tmp/tmp.depends; fi)
	(set +x; echo "(./test_depends <$(DATA_DIR)/depends 2>&1) | $(PADS_HOME)/scripts/remove_junk.pl | cat > tmp/tmp.depends")
	(set +x; (./test_depends <$(DATA_DIR)/depends 2>&1) | $(PADS_HOME)/scripts/remove_junk.pl | cat > tmp/tmp.depends)
	diff tmp/tmp.depends $(REGRESS_DIR)/test_depends.regress || echo TEST_DEPENDS DIFFERS
	(set +x; echo " ")

regress_test_struct3 : test_struct3
	(set +x; test -d tmp || mkdir tmp)
	(set +x; if [ -f tmp/tmp.struct3 ]; then $(RM) tmp/tmp.struct3; fi)
	(set +x; echo "(./test_struct3 <$(DATA_DIR)/struct3 2>&1) | $(PADS_HOME)/scripts/remove_junk.pl | cat > tmp/tmp.struct3")
	(set +x; (./test_struct3 <$(DATA_DIR)/struct3 2>&1) | $(PADS_HOME)/scripts/remove_junk.pl | cat > tmp/tmp.struct3)
	diff tmp/tmp.struct3 $(REGRESS_DIR)/test_struct3.regress || echo TEST_STRUCT3 DIFFERS
	(set +x; echo " ")

PFILES = $(sh cd $(P_DIR); (echo *.p | sed -e 's/[.]p/ /g'))

for pfile $(PFILES)
$(pfile).c $(pfile).h :JOINT: $(pfile).p $(PADSC) $(PADSC_REAL)
	(set +x; echo "Using rule P2")
	$(PADSC) $(*:N=*.p) -r $(GEN_DIR)
end

clean :
	$(RM) -f $(ALL_TESTS) *~ *.o ../*~ 

veryclean :
	$(RM) Makefile.mo Makefile.ms ../Makefile.mo ../Makefile.ms tmp/*

 # End of rules are run from the arch directory
end
