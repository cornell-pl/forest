
 # This is a GNU makefile.

PADS_HOME = .

TARGS = ast_lib padsc_lib
ifdef USE_GALAX
TARGS += pglx_lib
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

test_libpads:
	@(cd padsc; $(MAKE) test_libpads)

test_comp:
	@(cd padsc; $(MAKE) test_comp)

clean:
	@(cd padsc; $(MAKE) clean)

veryclean:
	@(cd padsc; $(MAKE) veryclean)

ast_lib:
	@(cp ast-ast/bin/package.cvs ast-ast/bin/package)
	@(cd ast-ast; ./bin/package read)
	@(cd ast-ast; ./bin/package make)
