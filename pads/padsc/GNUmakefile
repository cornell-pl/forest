
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
	@(cd libpads; $(MAKE) -f GNUMakefile install)

pglx_lib:
	echo Making pglx library
	@(cd libpglx; $(MAKE) -f GNUMakefile install)

pads_glx:
	echo Making pads-glx library
	@(cd pads-glx; $(MAKE) -f GNUMakefile install)

padsc_comp:
	echo Making padsc compiler
	@(cd comp; $(MAKE) -f GNUMakefile all)

regress:
	@(cd libpads/tests; $(MAKE) -f GNUMakefile regress; cd ../../examples/tests; $(MAKE) -f GNUMakefile regress) | $(PADS_HOME)/scripts/count_differs.pl

pglx_regress:
	@(cd tests_pglx; $(MAKE) -f GNUMakefile ) | $(PADS_HOME)/scripts/count_differs.pl

usecase_regress:
	@(cd usecases; $(MAKE) -f GNUMakefile ) | $(PADS_HOME)/scripts/count_differs.pl

perf_regress:
	@(cd perf; $(MAKE) -f GNUMakefile ) | $(PADS_HOME)/scripts/count_differs.pl

test_xml:
	@(cd examples/xml/tests; $(MAKE) -f GNUMakefile regress) 

test_libpads:
	@(cd libpads/tests; $(MAKE) -f GNUMakefile ) | $(PADS_HOME)/scripts/count_differs.pl

test_comp:
	@(cd examples/tests; $(MAKE) -f GNUMakefile ) | $(PADS_HOME)/scripts/count_differs.pl

clean:
	@(cd comp; $(MAKE) -f GNUMakefile clean)
	@(cd libpads; $(MAKE) -f GNUMakefile clean)
	@(if [ -d usecases ]; then cd usecases; $(MAKE) -f GNUMakefile clean; fi)
ifdef USE_GALAX
	@(cd pads-glx; $(MAKE) -f GNUMakefile clean)
	@(cd libpglx; $(MAKE) -f GNUMakefile clean)
endif

veryclean:
	@(cd comp; $(MAKE) -f GNUMakefile veryclean)
	@(cd libpads; $(MAKE) -f GNUMakefile veryclean)
	@(if [ -d usecases ]; then cd usecases; $(MAKE) -f GNUMakefile veryclean; fi)
ifdef USE_GALAX
	@(cd pads-glx; $(MAKE) -f GNUMakefile veryclean)
	@(cd libpglx; $(MAKE) -f GNUMakefile veryclean)
endif
