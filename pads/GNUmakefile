
 # This is a GNU makefile.

PADS_HOME = .

include $(PADS_HOME)/mk/rules.mk

TARGS = ast_lib padsc_lib
ifdef USE_GALAX
TARGS += pglx_lib
else
ifdef GEN_GALAX
TARGS += pglx_lib
endif
endif
TARGS += padsc_comp

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
	@(cp ast-ast/bin/package.cvs ast-ast/bin/package)
	@(cd ast-ast; ./bin/package read; ./bin/package make)
