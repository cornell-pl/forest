 # This is a GNU makefile.

ARCH_N_OPSYS = $(shell scripts/arch-n-opsys)

SML=sml

all:	compiler 

compiler:
	echo Making compiler
	sml < build-exec.sml

clean:
	$(RM) lib/padsc.$(ARCH_N_OPSYS)
	@(cd ckit; x="`find . -name \*~`"; if [ "$$x"x != x ]; then echo "In ckit, removing:\n$$x"; $(RM) $$x; fi)

veryclean: clean
	@(echo "Removing CM .bin files"; cd CM; x="`find . -name \*.bin`"; if [ "$$x"x != x ]; then echo "In CM, removing:\n$$x"; $(RM) $$x; fi)
	@(echo "Removing ckit .bin files"; cd ckit; x="`find . -name \*.bin`"; if [ "$$x"x != x ]; then echo "In ckit, removing:\n$$x"; $(RM) $$x; fi)
	@(echo "Removing util .bin files"; cd util; x="`find . -name \*.bin`"; if [ "$$x"x != x ]; then echo "In util, removing:\n$$x"; $(RM) $$x; fi)

redo_all:
	+@(for file in examples/p/*.p; do \
		echo ./padsc $$file; \
		./padsc $$file; \
	done;)
