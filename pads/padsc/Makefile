
 # This is an nmake file.  See GNUmakefile for the gmake version.

ARCH_N_OPSYS=$(sh scripts/arch-n-opsys)

SML=sml

all:	compiler 

compiler:
	echo Making compiler
	sml < build-exec.sml

clean:
	$(RM) -f lib/padsc.$(ARCH_N_OPSYS)
	(set +x; echo "Removing \*~ files"; x="`find . -name \*~`"; echo "x = $x"; if [ "$x"x != x ]; then echo "removing:\n$x"; $(RM) -f $x; fi)

veryclean: clean
	(set +x; echo "Removing CM .bin files"; cd CM; x="`find . -name \*.bin`"; if [ "$x"x != x ]; then echo "In CM, removing:\n$x"; $(RM) -f $x; fi)
	(set +x; echo "Removing ckit .bin files"; cd ckit; x="`find . -name \*.bin`"; echo "x = $x"; if [ "$x"x != x ]; then echo "In ckit, removing:\n$x"; $(RM) -f $x; fi)
	(set +x; echo "Removing util .bin files"; cd util; x="`find . -name \*.bin`"; if [ "$x"x != x ]; then echo "In util, removing:\n$x"; $(RM) -f $x; fi)
