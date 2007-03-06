######################################################################
# This is a GNU makefile
#
# Synopsis:
#
# 1. To compile the learning program, just type "make".
#    Note: This will not modify the script "learn". Instead, it
#          produces an updated file in the subdirectory "lib".
# 2. To run a test case, e.g. yum.txt, type "make yum.txt"
#    This will check if the output in the output/yum.txt directory
#    is up to date with respect the the input data/yum.txt. If
#    it is up to date, make will say so, otherwise it will run
#    the learning program to update output/yum.txt.
# 3. To check if a change affected an output, e.g. for the yum.txt
#    test case, type "make yum.txt.test". First it will run the
#    test case, as in case 2 above. Then it will check if the
#    output in output/yum.txt is different from the silver version
#    of the output in silver/yum.txt. It will report if there is
#    a difference or not.
# 4. To make a new silver version, e.g. for yum.txt, type
#    make "yum.txt.silver".
#
######################################################################

######################################################################
#
# Crib notes:
# $@ = name of target
# $< = name of first prerequisite
# $^ = name of all prerequisites
#
# make -C silver:   make in the directory silver
#
# $(notdir ../output/access_log/Ty) -> Ty
#
######################################################################

######################################################################
#
# Build infrastructure
#
######################################################################

ifndef SML
  export SML=sml
endif

SML_VER = $(shell $(SML) '@SMLversion' | sed -e 's|.* ||')
SML_MINOR_VER = $(shell echo $(SML_VER) | sed -e 's|.*[.]||')
ifeq ($(SML_VER),$(SML_MINOR_VER))
SML_MINOR_VER = 0
endif
SML_MINOR_VER_TEST=$(shell test $(SML_MINOR_VER) -gt 39 && echo GT39)
BUILD_EXEC = build-exec.sml

all:	learn

tokens.lex.sml: tokens.lex
	@echo Making learning program
	lexgen --ml-lex-mode tokens.lex

learn:  complexity.sml
learn:  distribution.sml
learn:  model.sml
learn:  config.sml
learn:  utils.sml
learn:  parseCmdLine-sig.sml
learn:  parseCmdLine.sml
learn:  tokens.lex.sml
learn:  types.sml
learn:  structure.sml
learn:  print.sml
learn:	main.sml
	@echo Making learning program
	$(SML) < $(BUILD_EXEC)
	@echo

######################################################################
#
# Silver version infrastructure
#
######################################################################

IND  = data
OUTD = output

# Silver targets
%.silver :
	@make $(*F).ag

%.ag :
	make $(*F)
	make -C silver $(*F)

DATA = ProfSec.log             \
       SCESETUP.log            \
       access_log              \
       ai-simple               \
       ai.3000                 \
#       apache.txt              \
#       asl.log                 \
       backup1.log             \
       backup2.log             \
       boot.log                \
       crashreporter.log       \
       daily.out               \
       dibbler.1000            \
       dmesg                   \
       error_log               \
       hp-array                \
       hp-struct               \
       ijnltif                 \
       install.log             \
       interactions.txt.small  \
       interface.loop          \
       lsof                    \
       netstat                 \
       netstat-an              \
       page_log                \
       quarterlypersonalincome \
       railroad.txt            \
       rpmpkgs                 \
       scesetup2.log           \
       scrollkeeper.log        \
       simplegroups            \
       simplexml               \
       windowserver_last.log   \
       yum.log                 \
       yum.txt

.PHONY: $(DATA)

1967Transactions.short: $(IND)/1967Transactions.short
	./learn $< -d $(OUTD)/$@ # quick

MER_T01_01.csv: $(IND)/MER_T01_01.csv
	./learn $< -d $(OUTD)/$@ # quick

ProfSec.log: $(IND)/ProfSec.log
	./learn $< -d $(OUTD)/$@ # quick

SCESETUP.LOG: $(IND)/SCESETUP.LOG
	./learn $< -d $(OUTD)/$@ # 30 minutes

access_log: $(IND)/access_log
	./learn $< -d $(OUTD)/$@ # 5 minutes

ai-simple: $(IND)/ai-simple
	./learn $< -d $(OUTD)/$@ # quick

ai.3000: $(IND)/ai.3000
	./learn $< -d $(OUTD)/$@ # several minutes

apache.txt: $(IND)/apache.txt
	./learn $< -d $(OUTD)/$@ -maxdepth 3 # Ran for more than four hours

asl.log: $(IND)/asl.log
	./learn $< -d $(OUTD)/$@ -maxdepth 3 # too long, even with maxdepth of 3

backup1.log: $(IND)/backup1.log
	./learn $< -d $(OUTD)/$@ # quick

backup2.log: $(IND)/backup2.log
	./learn $< -d $(OUTD)/$@ # quick

boot.log: $(IND)/boot.log
	./learn $< -d $(OUTD)/$@ # quick

crashreporter.log: $(IND)/crashreporter.log
	./learn $< -d $(OUTD)/$@ # one minute

daily.out: $(IND)/daily.out # two minutes with maxdepth of 3
	./learn $< -d $(OUTD)/$@ -maxdepth 4 # ten minutes with maxdepth of 4

dibbler.1000: $(IND)/dibbler.1000
	./learn $< -d $(OUTD)/$@ # quick

dmesg: $(IND)/dmesg
	./learn $< -d $(OUTD)/$@ -maxdepth 6 # quick with maxdepth of 5

error_log: $(IND)/error_log
	./learn $< -d $(OUTD)/$@ -maxdepth 4 # takes a while

hp-array: $(IND)/hp-array
	./learn $< -d $(OUTD)/$@ # quick

hp-struct: $(IND)/hp-struct
	./learn $< -d $(OUTD)/$@ # quick

ijnltif: $(IND)/ijnltif
	./learn $< -d $(OUTD)/$@ -maxdepth 3 # too long, even with maxdepth at 3

install.log: $(IND)/install.log
	./learn $< -d $(OUTD)/$@ -maxdepth 7 # tolerable but long at maxdepth 7

# **** Generates error with maxdepth=7
interactions.txt.small: $(IND)/interactions.txt.small
	./learn $< -d $(OUTD)/$@ -maxdepth 6 # tolerable at maxdepth 6

interface.loop: $(IND)/interface.loop
	./learn $< -d $(OUTD)/$@ -maxdepth 4 # ten minutes at maxdepth of 4

lsof: $(IND)/lsof
	./learn $< -d $(OUTD)/$@ -maxdepth 4 # five minutes at maxdepth of 5

netstat: $(IND)/netstat
	./learn $< -d $(OUTD)/$@ # quick

netstat-an: $(IND)/netstat-an
	./learn $< -d $(OUTD)/$@ # quick

page_log: $(IND)/page_log
	./learn $< -d $(OUTD)/$@ # quick

quarterlypersonalincome: $(IND)/quarterlypersonalincome
	./learn $< -d $(OUTD)/$@ # quick

railroad.txt: $(IND)/railroad.txt
	./learn $< -d $(OUTD)/$@ # 15 minutes

rpmpkgs: $(IND)/rpmpkgs
	./learn $< -d $(OUTD)/$@ # quick

scesetup2.log: $(IND)/scesetup2.log
	./learn $< -d $(OUTD)/$@ # finished, lost track of how long

scrollkeeper.log: $(IND)/scrollkeeper.log
	./learn $< -d $(OUTD)/$@ # quick

simplegroups: $(IND)/simplegroups  # quick
	cd output; make $@

simplexml: $(IND)/simplexml
	cd output; make $@  # very quick

windowserver_last.log: $(IND)/windowserver_last.log
	cd output; make $@ # three minutes

yum.log: $(IND)/yum.log;
	cd output; make $@  # very quick

yum.txt: $(IND)/yum.txt;
	cd output; make $@   # quick

######################################################################
#
# Silver version infrastructure
#
######################################################################

# Test against a silver version
%.test:
	make -C test $(*F)

######################################################################
#
# Utility targets
#
######################################################################

# Print out versions of stuff
.PHONY : foo
foo:
	@(echo SML = $(SML))
	@(echo SML_VER = $(SML_VER))
	@(echo SML_MINOR_VER = $(SML_MINOR_VER))
	@(echo SML_MINOR_VER_TEST = $(SML_MINOR_VER_TEST))
	@(echo BUILD_EXEC = $(BUILD_EXEC))

.PHONY : clean
clean:
	$(RM) lib/learn.$(ARCH_N_OPSYS)
	$(RM) tokens.lex.sml
	$(RM) *~

.PHONY : veryclean
veryclean: clean
	@(echo "Removing .cm .bin files"; if [ -d .cm ]; then cd .cm; x="`find . -name \*.bin`"; if [ "$$x"x != x ]; then echo "In .cm, removing:\n$$x"; $(RM) $$x; fi; fi)
	@(echo "Removing ckit .bin files"; cd ckit; x="`find . -name \*.bin`"; if [ "$$x"x != x ]; then echo "In ckit, removing:\n$$x"; $(RM) $$x; fi)
	@(echo "Removing util .bin files"; cd util; x="`find . -name \*.bin`"; if [ "$$x"x != x ]; then echo "In util, removing:\n$$x"; $(RM) $$x; fi)
