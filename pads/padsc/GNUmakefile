
 # This is a GNU makefile.

TARGS = padsc_lib
ifdef USE_GALAX
TARGS += pglx_lib 
else
ifdef GEN_GALAX
TARGS += pglx_lib 
endif
endif
TARGS += padsc_comp

all:	$(TARGS)

padsc_lib:
	echo Making padsc library
	@(cd libpadsc; $(MAKE) install)

pglx_lib:
	echo Making pglx library
	@(cd libpglx; $(MAKE) install)

padsc_comp:
	echo Making padsc compiler
	@(cd comp; $(MAKE) all)

regress: test_libpadsc test_comp

test_libpadsc:
	@(cd libpadsc/tests; $(MAKE))

test_comp:
	@(cd examples/tests; $(MAKE))

clean:
	@(cd libpadsc; $(MAKE) clean)
	@(cd libpglx; $(MAKE) clean)
	@(cd comp; $(MAKE) clean)

veryclean:
	@(cd libpadsc; $(MAKE) veryclean)
	@(cd libpglx; $(MAKE) veryclean)
	@(cd comp; $(MAKE) veryclean)
