
 # This is a GNU makefile.

ifndef PADS_HOME
  export PADS_HOME=$(shell cd ../../..; pwd)
endif

all:
	@(cd src; $(MAKE))

regress:
	@(cd src; $(MAKE) regress)

clean:
	@(cd src; $(MAKE) clean)

veryclean:
	@(cd src; $(MAKE) veryclean)
