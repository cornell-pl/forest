PADS_HOME = .

include $(PADS_HOME)/mk/rules.mk

TARGS = padsc_lib
ifdef GALAX_GEN
TARGS += pglx_lib
else
ifdef USE_GALAX
TARGS += pglx_lib
endif
endif
TARGS += padsc_comp

foo:
	echo TARGS $(TARGS)
 
all: $(TARGS)

padsc_lib:
	@(cd padsc; $(MAKE) padsc_lib)

pglx_lib:
	@(cd padsc; $(MAKE) pglx_lib)

padsc_comp:
	@(cd padsc; $(MAKE) padsc_comp)

regress:
	@(cd padsc; $(MAKE) regress)

test_libpadsc:
	@(cd padsc; $(MAKE) test_libpadsc)

test_comp:
	@(cd padsc; $(MAKE) test_comp)

clean:
	@(cd padsc; $(MAKE) clean)

veryclean:
	@(cd padsc; $(MAKE) veryclean)

# This should only be done once
ast_lib:
	@(cd ast-base; ./bin/package make || echo "make failed but libast may have been built")
