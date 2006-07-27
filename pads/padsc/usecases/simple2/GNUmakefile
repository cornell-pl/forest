
 # This is a GNU makefile.

ifndef PADS_HOME
  export PADS_HOME=$(shell cd ../../..; pwd)
endif

all:
	@(cd src; $(MAKE) -f GNUMakefile )

regress:
	@(cd src; $(MAKE) -f GNUMakefile regress)

clean:
	@(cd src; $(MAKE) -f GNUMakefile clean)

veryclean:
	@(cd src; $(MAKE) -f GNUMakefile veryclean)
