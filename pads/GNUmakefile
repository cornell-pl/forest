
 # This is a GNU makefile.

ifndef PADS_HOME
%: forceabort2
	@echo "ERROR: env variable PADS_HOME must be defined"
	@exit 1
forceabort2: ;
endif

TARGS = ast_lib padsc_lib
ifdef USE_GALAX
TARGS += pglx_lib
endif
TARGS += padsc_comp

ifndef AST_ARCH
AST_ARCH := $(shell $(PADS_HOME)/ast-ast/bin/package.cvs)
export AST_ARCH
endif
AST_LIB = ast-ast/arch/$(AST_ARCH)/lib/libast.a

all: $(TARGS)

ast_lib : $(AST_LIB)

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

veryveryclean: veryclean
	@(cd ast-ast/arch; $(RM) -rf `../bin/package`)
	@($(RM) ./mk/rules.arch.`ast-ast/bin/package`.mk)

WHERE_NMAKE := $(shell (type nmake 2>/dev/null) | grep -v 'not found')
ifeq ($(WHERE_NMAKE),)
$(AST_LIB) : ast-ast/lib/package/tgz/*.tgz
	@(if [ ! -f ast-ast/bin/package ]; then cp ast-ast/bin/package.cvs ast-ast/bin/package; fi)
	@(cd ast-ast; ./bin/package read)
	@(cd ast-ast; ./bin/package make)
else
$(AST_LIB) : ast-ast/lib/package/tgz/ast-ast*.tgz
	@(echo " "; echo "*** To build ast_lib, nmake must NOT be in your path."; \
		echo "      (\`type nmake\` ==> $(WHERE_NMAKE))"; \
		echo "    Change your PATH env variable and try again."; echo " ")
	@(exit -1)
endif
