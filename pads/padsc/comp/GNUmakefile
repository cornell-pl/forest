
 # This is a GNU makefile

ifndef PADS_HOME
%: forceabort2
	@echo "ERROR: env variable PADS_HOME must be defined"
	@exit 1
forceabort2: ;
endif




ARCH_N_HEAPOPSYS := $(shell $(PADS_HOME)/scripts/arch-n-heapopsys)

SML = $(PADS_HOME)/binlinks/sml.$(ARCH_N_HEAPOPSYS)
BUILDEXEC = build-exec-45.sml

SMLVER=$(shell $(SML) '@SMLversion' | sed -e 's|.* ||')

ifeq ($(SMLVER),110)
BUILDEXEC = build-exec-45.sml
endif

ifeq ($(SMLVER),110.19)
BUILDEXEC = build-exec.sml
endif

all:	compiler

foo:
	@(echo SMLVER = $(SMLVER))
	@(echo BUILDEXEC = $(BUILDEXEC))

compiler:
	echo Making compiler
	echo $(SMLVER)
	$(SML) < build-exec.sml

compiler45:
	echo Making compiler
	echo $(SMLVER)
	$(SML) < build-exec-45.sml

clean:
	$(RM) $(PADS_HOME)/lib/padsc.$(ARCH_N_OPSYS)
	@(cd ckit; x="`find . -name \*~`"; if [ "$$x"x != x ]; then echo "In ckit, removing:\n$$x"; $(RM) $$x; fi)

veryclean: clean
	@(echo "Removing CM .bin files"; cd CM; x="`find . -name \*.bin`"; if [ "$$x"x != x ]; then echo "In CM, removing:\n$$x"; $(RM) $$x; fi)
	@(echo "Removing ckit .bin files"; cd ckit; x="`find . -name \*.bin`"; if [ "$$x"x != x ]; then echo "In ckit, removing:\n$$x"; $(RM) $$x; fi)
	@(echo "Removing util .bin files"; cd util; x="`find . -name \*.bin`"; if [ "$$x"x != x ]; then echo "In util, removing:\n$$x"; $(RM) $$x; fi)

redo_all:
	+@(for file in examples/p/*.p; do \
		echo $(PADS_HOME)/scripts/padsc $$file; \
		$(PADS_HOME)/scripts/padsc $$file; \
	done;)
