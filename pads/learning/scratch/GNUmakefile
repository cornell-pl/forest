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
# 5. To run all the outputs, type "make outputs"
# 6. To make a summary report on the most recent version, the
#    silver and golden versions, run the target with a ".rep"
#    suffix, eg. "make quarterlypersonalincome.rep"
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

vanilla.lex:
	config.pl vanilla.config

tokens.lex.sml: tokens.lex
	@echo Running lexer generator
	lexgen --ml-lex-mode tokens.lex

vanilla.lex.sml: vanilla.lex
	@echo Running lexer generator
	lexgen --ml-lex-mode vanilla.lex

learn:  common.sml
learn:  complexity.sml
learn:  config.sml
learn:  distribution.sml
learn:  functionaldep.sml
learn:  gold.sml
learn:  hosts.sml
learn:	main.sml
learn:  matrix.sml
learn:  model.sml
learn:  partition.sml
learn:  options.sml
learn:  parseCmdLine.sml
learn:  parseCmdLine-sig.sml
learn:  populate.sml
learn:  print.sml
learn:  rat.sml
learn:  reduce.sml
learn:  rewrite.sml
learn:  structure.sml
learn:  vanilla.lex.sml
learn:  table.sml
learn:  times.sml
learn:  tokens.lex.sml
learn:  types.sml
learn:  utils.sml
	@echo Making learning program
	nice -n20 $(SML) < $(BUILD_EXEC)
	@echo

######################################################################
#
# Silver version infrastructure
#
######################################################################

IND  = data
OUTD = output
# Data files, omitting those that take too long
# SCESETUP.LOG generates error on Empty
DATA := $(shell ls data | egrep -v "goldStandard|CVS|README" | egrep -v "SCESETUP.LOG apache.txt")
SILVERS  = $(addsuffix .ag,$(DATA))
REPORTS  = $(addsuffix .rep,$(DATA))
XS       = $(addsuffix .x,x $(DATA))
TESTS    = $(addsuffix .test,$(DATA))

.PHONY: $(DATA)
.PHONY: x

$(XS): %.x: %
	echo $^

silvers: $(SILVERS)
	echo Making silver version of each data file output

reports: $(REPORTS)

define silver-it
@if [ ! -d silver/$(*F) ] ; then mkdir silver/$(*F); fi
cp output/$(*F)/Ty silver/$(*F);
cp output/$(*F)/Params silver/$(*F);
cp output/$(*F)/Complexity silver/$(*F);
cp output/$(*F)/Timing silver/$(*F);
echo "Sleep for four seconds";
sleep 4;
endef

# The silver file depends on the corresponding output file.
$(SILVERS): %.ag: %
	$(silver-it)

# %.silver :    ; $(silver-it)
# %.ag     :    ; $(silver-it)

define learn-it
# Clean the gen directory
rm -rf gen
mkdir gen
# Make an output directory if one is needed
if [ ! -d output/$@ ] ; then mkdir output/$@; fi
# Clean the output directory
if [ -e output/$@/* ] ; then rm -rf output/$@/*; fi
# 1. Run the inference algorithm
nice -n20 ./learn -n $@ -lex vanilla data/$@ | tee gen/$@.out
cp -R gen/* output/$@
cp vanilla.p gen
endef

define summary
# 1. Run the inference algorithm
#    Already done by "learn-it", owing to dependency on output file
# NOT DONE: compare score of generated silver description with associated golden standard.
# NOT DONE: compare score of generated silver description with previous generated silver description.
# 4. generate PADS description
# 5. generate accumulator program
make --directory=gen $(*F)-accum
#   preserve the gen directory
cp -R gen/* output/$(*F)
# 6. run accumulator program on input data
gen/darwin.ppc/$(*F)-accum data/$(*F) 2>gen/$(*F)-accum.out
# 7. extract number of records correctly parsed and number of errors
accum-out.pl gen/$(*F)-accum.out | tee gen/ParseReport
# 9. collect in a report file:
#    a. how large the input file is in bytes and number of records
@echo "Record count = " `cat data/$(*F) | wc -l` | tee gen/RecordCount
@echo "Byte count =   " `cat data/$(*F) | wc -c` | tee gen/ByteCount
#    b. score of the new silver
@cat output/$(*F)/Complexity
#    c. score of the old silver
@cat silver/$(*F)/Complexity
#    e. number of correctly parsed records
#    f. number of incorrectly parsed records
#    SEE STEP 7 ABOVE
# Assemble the report
echo "DATA FILE: " $(*F) >>gen/Report
echo >>gen/Report
cat gen/RecordCount >>gen/Report
cat gen/ByteCount >>gen/Report
echo >>gen/Report
echo "Unrefined complexity scores" >>gen/Report
cat gen/BaseComplexity >>gen/Report
echo "New complexity scores" >>gen/Report
cat gen/Complexity >>gen/Report
echo "Silver complexity scores" >>gen/Report
cat silver/$(*F)/Complexity >>gen/Report
echo >>gen/Report
cat gen/GoldComplexity >>gen/Report
cat gen/ParseReport >>gen/Report
echo >>gen/Report
cat gen/Timing >>gen/Report
endef

# The summary file depends on the corresponding output file.
$(REPORTS): %.rep: %
	$(summary)

######################################################################
# Target to run just inference algorithm
######################################################################
outputs : $(DATA)

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
interactions.txt.small:  $(IND)/interactions.txt.small  ; $(learn-it)
interface.loop:          $(IND)/interface.loop          ; $(learn-it)
ls-l.txt:                $(IND)/ls-l.txt                ; $(learn-it)
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
@if ! cmp --quiet output/yum.txt/Params silver/yum.txt/Params ; \
 then echo $@/Params has changed since the silver version     ; \
      diff output/yum.txt/Params silver/yum.txt/Params        ; \
      echo; \
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
	$(RM) tokens.lex.sml vanilla.lex.sml vanilla.lex
	$(RM) *~

.PHONY : veryclean
veryclean: clean
	@(echo "Removing .cm .bin files"; if [ -d .cm ]; then cd .cm; x="`find . -name \*.bin`"; if [ "$$x"x != x ]; then echo "In .cm, removing:\n$$x"; $(RM) $$x; fi; fi)
	@(echo "Removing ckit .bin files"; cd ckit; x="`find . -name \*.bin`"; if [ "$$x"x != x ]; then echo "In ckit, removing:\n$$x"; $(RM) $$x; fi)
	@(echo "Removing util .bin files"; cd util; x="`find . -name \*.bin`"; if [ "$$x"x != x ]; then echo "In util, removing:\n$$x"; $(RM) $$x; fi)
