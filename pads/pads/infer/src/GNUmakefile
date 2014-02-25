######################################################################
#
# GNUMake file for building learning system
#
# Assumes that PADS has been installed and the PADS_HOME
# environment variable has been set to the root PADS directory
#
######################################################################

######################################################################
#
# Build infrastructure
#
######################################################################

ifndef AST_ARCH
  export AST_ARCH=$(shell $(PADS_HOME)/ast-ast/bin/package.cvs)
endif

ifndef ARCH_N_OPSYS
 export ARCH_N_OPSYS=$(shell $(PADS_HOME)/scripts/arch-n-opsys)
endif

ifndef SML
  export SML=$(shell which sml)
endif

ifndef SML_PATH
  SML_PATH = $(SML)
endif
ifneq ("$(AST_ARCH)","darwin.ppc")
  SML_PATH = $(SML) @SMLalloc=1M
endif

SML_ROOT_PATH=`dirname $(SML)`
SML_VER = $(shell $(SML) '@SMLversion' | sed -e 's|.* ||')
SML_MINOR_VER = $(shell echo $(SML_VER) | sed -e 's|.*[.]||')
ifeq ($(SML_VER),$(SML_MINOR_VER))
SML_MINOR_VER = 0
endif
SML_MINOR_VER_TEST=$(shell test $(SML_MINOR_VER) -gt 39 && echo GT39)
BUILD_EXEC = build-exec.sml
BUILD_INC = build-inc.sml
BUILD_DIST = build-dist.sml

######################################################################
#
# Learning tool
#
######################################################################


all:	learn

vanilla.lex: ../include/vanilla.config ../scripts/config.pl
	../scripts/config.pl ../include/vanilla.config .
	mv vanilla.p ../include/vanilla.p

walaa.lex: ../include/walaa.config ../scripts/config.pl
	../scripts/config.pl ../include/walaa.config .
	mv walaa.p ../include/walaa.p

tokens.lex: ../scripts/config.pl ../include/tokens.config
	../scripts/config.pl ../include/tokens.config .
	mv tokens.p ../include/tokens.p

tokens.lex.sml: tokens.lex
	@echo Running lexer generator
	$(SML_ROOT_PATH)/ml-ulex --ml-lex-mode tokens.lex

vanilla.lex.sml: vanilla.lex
	@echo Running lexer generator
	$(SML_ROOT_PATH)/ml-ulex --ml-lex-mode vanilla.lex

walaa.lex.sml: walaa.lex
	@echo Running lexer generator
	$(SML_ROOT_PATH)/ml-ulex --ml-lex-mode walaa.lex

pxml.lex.sml: pxml.lex
	@echo Running lexer generator for pxml
	$(SML_ROOT_PATH)/ml-ulex pxml.lex

pxml.grm.sml: pxml.grm
	@echo Running parser generator for pxml
	$(SML_ROOT_PATH)/ml-antlr pxml.grm

learn:  tokens.lex.sml vanilla.lex.sml
	@echo Making learning program
	nice -n20 $(SML_PATH) < $(BUILD_EXEC)
	@echo
increment: tokens.lex.sml vanilla.lex.sml pxml.lex.sml pxml.grm.sml
	@echo Making the increment program
	nice -n20 $(SML_PATH) < $(BUILD_INC)
	@echo
descdist: editdistance.sml descdist.sml
	@echo Making the descdist program
	nice -n20 $(SML_PATH) < $(BUILD_DIST)
	@echo

######################################################################
#
# Utility targets
#
######################################################################

# Print out versions of stuff
.PHONY : versions
versions:
	@(echo SML = $(SML))
	@(echo SML_PATH = $(SML_PATH))
	@(echo SML_ROOT_PATH = $(SML_ROOT_PATH))
	@(echo SML_VER = $(SML_VER))
	@(echo SML_MINOR_VER = $(SML_MINOR_VER))
	@(echo SML_MINOR_VER_TEST = $(SML_MINOR_VER_TEST))
	@(echo BUILD_EXEC = $(BUILD_EXEC))
	@(echo AST_ARCH = $(AST_ARCH))
	@(echo ARCH_N_OPSYS = $(ARCH_N_OPSYS))

.PHONY : clean
clean:
	$(RM) lib/learn.$(ARCH_N_OPSYS)
	$(RM) tokens.lex tokens.lex.sml vanilla.lex vanilla.lex.sml pxml.lex.sml pxml.grm.sml
	$(RM) ../include/tokens.p ../include/vanilla.p
	$(RM) *~
	$(RM) .DS_Store

.PHONY : veryclean
veryclean: clean
	@(echo "Removing .cm files"; if [ -d .cm ]; then cd .cm; x="`find . -name .cm`"; if [ "$$x"x != x ]; then echo "In .cm, removing:\n$$x"; $(RM) -rf $$x; fi; fi)

