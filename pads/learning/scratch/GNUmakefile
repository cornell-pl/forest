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

%.silver :
	@make $(*F).ag

%.ag :
	make $(*F)
	make -C silver $(*F)

DATA = 1967Transactions.short  \
       MER_T01_01.csv          \
       ProfSec.log             \
       access_log              \
       ai-simple               \
       ai.3000                 \
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
# The following currently take too long
#       SCESETUP.log            \
#       apache.txt              \
#       asl.log                 \
#       ijnltif                 \

SILVERS = $(addsuffix .ag,$(DATA))

silvers: $(SILVERS)
	echo Making silver version of each data file output

define learn-it
./learn data/$@ -d output/$@ `maxdepth.pl $@`
touch output/$@
endef

define silver-it
@if [ ! -d silver/$@ ] ; then mkdir silver/$@; cvs add silver/$@; fi
@if [ -e silver/$< ] ; then cp $^ silver/$@ ; else cp $^ silver/$@; cd silver/$@; cvs add $(notdir $^); fi
endef

.PHONY: $(DATA)

1967Transactions.short:  $(IND)/1967Transactions.short  ; $(learn-it)
MER_T01_01.csv:          $(IND)/MER_T01_01.csv          ; $(learn-it)
ProfSec.log:             $(IND)/ProfSec.log             ; $(learn-it)
SCESETUP.LOG:            $(IND)/SCESETUP.LOG            ; $(learn-it)
access_log:              $(IND)/access_log              ; $(learn-it)
ai-simple:               $(IND)/ai-simple               ; $(learn-it)
ai.3000:                 $(IND)/ai.3000                 ; $(learn-it)
apache.txt:              $(IND)/apache.txt              ; $(learn-it)
asl.log:                 $(IND)/asl.log                 ; $(learn-it)
backup1.log:             $(IND)/backup1.log             ; $(learn-it)
backup2.log:             $(IND)/backup2.log             ; $(learn-it)
boot.log:                $(IND)/boot.log                ; $(learn-it)
crashreporter.log:       $(IND)/crashreporter.log       ; $(learn-it)
daily.out:               $(IND)/daily.out               ; $(learn-it)
dibbler.1000:            $(IND)/dibbler.1000            ; $(learn-it)
dmesg:                   $(IND)/dmesg                   ; $(learn-it)
error_log:               $(IND)/error_log               ; $(learn-it)
hp-array:                $(IND)/hp-array                ; $(learn-it)
hp-struct:               $(IND)/hp-struct               ; $(learn-it)
ijnltif:                 $(IND)/ijnltif                 ; $(learn-it)
install.log:             $(IND)/install.log             ; $(learn-it)
# **** Generates error with maxdepth=7
interactions.txt.small:  $(IND)/interactions.txt.small  ; $(learn-it)
interface.loop:          $(IND)/interface.loop          ; $(learn-it)
lsof:                    $(IND)/lsof                    ; $(learn-it)
netstat:                 $(IND)/netstat                 ; $(learn-it)
netstat-an:              $(IND)/netstat-an              ; $(learn-it)
page_log:                $(IND)/page_log                ; $(learn-it)
quarterlypersonalincome: $(IND)/quarterlypersonalincome ; $(learn-it)
railroad.txt:            $(IND)/railroad.txt            ; $(learn-it)
rpmpkgs:                 $(IND)/rpmpkgs                 ; $(learn-it)
scesetup2.log:           $(IND)/scesetup2.log           ; $(learn-it)
scrollkeeper.log:        $(IND)/scrollkeeper.log        ; $(learn-it)
simplegroups:            $(IND)/simplegroups            ; $(learn-it)
simplexml:               $(IND)/simplexml               ; $(learn-it)
windowserver_last.log:   $(IND)/windowserver_last.log   ; $(learn-it)
yum.log:                 $(IND)/yum.log                 ; $(learn-it)
yum.txt:                 $(IND)/yum.txt                 ; $(learn-it)

######################################################################
#
# Silver version infrastructure
#
######################################################################

# Test against a silver version
define test-it
make $(*F)
@if ! cmp --quiet output/yum.txt/Ty silver/yum.txt/Ty ; \
 then echo $@/Ty has changed since the silver version ; \
      diff output/yum.txt/Ty silver/yum.txt/Ty        ; \
      echo; \
 fi
@if ! cmp --quiet output/yum.txt/Params silver/yum.txt/Params ;\
 then echo $@/Params has changed since the silver version     ;\
      diff output/yum.txt/Params silver/yum.txt/Params        ; \
 fi
@if cmp --quiet output/yum.txt/Params silver/yum.txt/Params &&   \
    cmp --quiet output/yum.txt/Ty silver/yum.txt/Ty             ;\
 then echo No changes for test case $@ since the silver version ;\
 fi
endef

%.test:   ; $(test-it)

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
