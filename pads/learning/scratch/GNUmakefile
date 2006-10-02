
 # This is a GNU makefile

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

foo:
	@(echo SML = $(SML))
	@(echo SML_VER = $(SML_VER))
	@(echo SML_MINOR_VER = $(SML_MINOR_VER))
	@(echo SML_MINOR_VER_TEST = $(SML_MINOR_VER_TEST))
	@(echo BUILD_EXEC = $(BUILD_EXEC))

learn:	main.sml tokens.lex
	echo Making learning program
	lexgen --ml-lex-mode tokens.lex
	$(SML) < $(BUILD_EXEC)

clean:
	$(RM) lib/learn.$(ARCH_N_OPSYS)

veryclean: clean
	@(echo "Removing .cm .bin files"; if [ -d .cm ]; then cd .cm; x="`find . -name \*.bin`"; if [ "$$x"x != x ]; then echo "In .cm, removing:\n$$x"; $(RM) $$x; fi; fi)
	@(echo "Removing ckit .bin files"; cd ckit; x="`find . -name \*.bin`"; if [ "$$x"x != x ]; then echo "In ckit, removing:\n$$x"; $(RM) $$x; fi)
	@(echo "Removing util .bin files"; cd util; x="`find . -name \*.bin`"; if [ "$$x"x != x ]; then echo "In util, removing:\n$$x"; $(RM) $$x; fi)

