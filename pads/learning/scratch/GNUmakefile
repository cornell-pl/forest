######################################################################
# This is a GNU makefile
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
# Testing infrastructure
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
       asl.log                 \
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

1967Transactions.short: learn
1967Transactions.short: $(IND)/1967Transactions.short
	./learn $< -d $(OUTD)/$@

MER_T01_01.csv: learn
MER_T01_01.csv: $(IND)/MER_T01_01.csv
	./learn $< -d $(OUTD)/$@

ProfSec.log: learn
ProfSec.log: $(IND)/ProfSec.log
	./learn $< -d $(OUTD)/$@

SCESETUP.LOG: learn # 30 minutes
SCESETUP.LOG: $(IND)/SCESETUP.LOG
	./learn $< -d $(OUTD)/$@

access_log: learn # 5 minutes
access_log: $(IND)/access_log
	./learn $< -d $(OUTD)/$@

ai-simple: learn # quick
ai-simple: $(IND)/ai-simple
	./learn $< -d $(OUTD)/$@

ai.3000: learncvs 
ai.3000: $(IND)/ai.3000
	./learn $< -d $(OUTD)/$@

# Takes really really long time without maxdepth of 3
apache.txt: learn # Takes too long
apache.txt: $(IND)/apache.txt
	./learn $< -d $(OUTD)/$@ -maxdepth 3

asl.log: learn
asl.log: $(IND)/asl.log
	./learn $< -d $(OUTD)/$@

backup1.log: learn
backup1.log: $(IND)/backup1.log
	./learn $< -d $(OUTD)/$@

backup2.log: learn
backup2.log: $(IND)/backup2.log
	./learn $< -d $(OUTD)/$@

boot.log: learn
boot.log: $(IND)/boot.log
	./learn $< -d $(OUTD)/$@

crashreporter.log: learn # one minute
crashreporter.log: $(IND)/crashreporter.log
	./learn $< -d $(OUTD)/$@

daily.out: learn
daily.out: $(IND)/daily.out # two minutes with maxdepth of 3
	./learn $< -d $(OUTD)/$@ -maxdepth 4 # ten minutes with maxdepth of 4

dibbler.1000: learn # 
dibbler.1000: $(IND)/dibbler.1000
	./learn $< -d $(OUTD)/$@

dmesg: learn
dmesg: $(IND)/dmesg # quick with maxdepth of 5
	./learn $< -d $(OUTD)/$@ -maxdepth 6

error_log: learn # takes a while
error_log: $(IND)/error_log
	./learn $< -d $(OUTD)/$@ -maxdepth 4

hp-array: learn # quick
hp-array: $(IND)/hp-array
	./learn $< -d $(OUTD)/$@

hp-struct: learn # quick
hp-struct: $(IND)/hp-struct
	./learn $< -d $(OUTD)/$@

ijnltif: learn # too long, even with maxdepth at 3
ijnltif: $(IND)/ijnltif
	./learn $< -d $(OUTD)/$@ -maxdepth 3

install.log: learn # too long without maxdepth specified
install.log: $(IND)/install.log
	./learn $< -d $(OUTD)/$@ -maxdepth 7

interactions.txt.small: learn # too long without maxdepth specified
interactions.txt.small: $(IND)/interactions.txt.small
	./learn $< -d $(OUTD)/$@ -maxdepth 5

interface.loop: learn
interface.loop: $(IND)/interface.loop
	./learn $< -d $(OUTD)/$@

lsof: learn
lsof: $(IND)/lsof
	./learn $< -d $(OUTD)/$@

netstat: learn
netstat: $(IND)/netstat
	./learn $< -d $(OUTD)/$@

netstat-an: learn
netstat-an: $(IND)/netstat-an
	./learn $< -d $(OUTD)/$@

page_log: learn
page_log: $(IND)/page_log
	./learn $< -d $(OUTD)/$@

quarterlypersonalincome: learn
quarterlypersonalincome: $(IND)/quarterlypersonalincome
	./learn $< -d $(OUTD)/$@

railroad.txt: learn
railroad.txt: $(IND)/railroad.txt
	./learn $< -d $(OUTD)/$@

rpmpkgs: learn
rpmpkgs: $(IND)/rpmpkgs
	./learn $< -d $(OUTD)/$@

scesetup2.log: learn
scesetup2.log: $(IND)/scesetup2.log
	./learn $< -d $(OUTD)/$@

scrollkeeper.log: learn
scrollkeeper.log: $(IND)/scrollkeeper.log
	./learn $< -d $(OUTD)/$@

simplegroups: learn
simplegroups: $(IND)/simplegroups
	./learn $< -d $(OUTD)/$@

simplexml: learn
simplexml: $(IND)/simplexml
	./learn $< -d $(OUTD)/$@

windowserver_last.log: learn
windowserver_last.log: $(IND)/windowserver_last.log
	./learn $< -d $(OUTD)/$@

yum.log: learn
yum.log: $(IND)/yum.log
	./learn $< -d $(OUTD)/$@

yum.txt: learn
yum.txt: $(IND)/yum.txt
	./learn $< -d $(OUTD)/$@

# Test against a silver version
# ***** Add date to Ty and Params
%.test :

#%::

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
