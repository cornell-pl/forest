
 # This is a GNU makefile.

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

regress: test_libpads test_comp

test_libpads:
	@(cd libpads/tests; $(MAKE))

test_comp:
	@(cd examples/tests; $(MAKE))

clean:
	@(cd libpads; $(MAKE) clean)
	@(cd libpglx; $(MAKE) clean)
	@(cd comp; $(MAKE) clean)

veryclean:
	@(cd libpads; $(MAKE) veryclean)
	@(cd libpglx; $(MAKE) veryclean)
	@(cd comp; $(MAKE) veryclean)
