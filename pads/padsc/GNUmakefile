
 # This is a GNU makefile.

ifndef PADS_HOME
%: forceabort2
	@echo "ERROR: env variable PADS_HOME must be defined"
	@exit 1
forceabort2: ;
endif

TARGS = padsc_lib
ifdef USE_GALAX
TARGS += pglx_lib 
endif
TARGS += padsc_comp

all:	$(TARGS)

padsc_lib:
	echo Making padsc library
	@(cd libpads; $(MAKE) install)

pglx_lib:
	echo Making pglx library
	@(cd libpglx; $(MAKE) install)

padsc_comp:
	echo Making padsc compiler
	@(cd comp; $(MAKE) all)

regress:
	@(cd libpads/tests; $(MAKE) regress; cd ../../examples/tests; $(MAKE) regress) | $(PADS_HOME)/scripts/count_differs.pl

test_libpads:
	@(cd libpads/tests; $(MAKE)) | $(PADS_HOME)/scripts/count_differs.pl

test_comp:
	@(cd examples/tests; $(MAKE)) | $(PADS_HOME)/scripts/count_differs.pl

clean:
	@(cd libpads; $(MAKE) clean)
	@(cd libpglx; $(MAKE) clean)
	@(cd comp; $(MAKE) clean)

veryclean:
	@(cd libpads; $(MAKE) veryclean)
	@(cd libpglx; $(MAKE) veryclean)
	@(cd comp; $(MAKE) veryclean)
