
 # This is a GNU makefile.

ifndef PADS_HOME
  export PADS_HOME=$(shell cd ..; pwd)
endif

TARGS = padsc_lib
ifdef USE_GALAX
TARGS +=  pads_glx pglx_lib
endif
TARGS += padsc_comp

all:	$(TARGS)

padsc_lib:
	echo Making padsc library
	@(cd libpads; $(MAKE) install)

pglx_lib:
	echo Making pglx library
	@(cd libpglx; $(MAKE) install)

pads_glx:
	echo Making pads-glx library
	@(cd pads-glx; $(MAKE) install)

padsc_comp:
	echo Making padsc compiler
	@(cd comp; $(MAKE) all)

regress:
	@(cd libpads/tests; $(MAKE) regress; cd ../../examples/tests; $(MAKE) regress) | $(PADS_HOME)/scripts/count_differs.pl

test_xml:
	@(cd examples/xml/tests; $(MAKE) regress) 

test_libpads:
	@(cd libpads/tests; $(MAKE)) | $(PADS_HOME)/scripts/count_differs.pl

test_comp:
	@(cd examples/tests; $(MAKE)) | $(PADS_HOME)/scripts/count_differs.pl

clean:
	@(cd libpads; $(MAKE) clean)
	@(cd pads-glx; $(MAKE) clean)
	@(cd libpglx; $(MAKE) clean)
	@(cd comp; $(MAKE) clean)

veryclean:
	@(cd libpads; $(MAKE) veryclean)
	@(cd pads-glx; $(MAKE) veryclean)
	@(cd libpglx; $(MAKE) veryclean)
	@(cd comp; $(MAKE) veryclean)
