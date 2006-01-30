 # This makefile uses gmake extensions
 # such as $(shell ...) to execute a command

.PRECIOUS: %.c

# These variables need to be set
ifndef PADS_HOME
  PADS_HOME=/home/kfisher/pads
endif

# These variables should not need to change
PADSC=$(PADS_HOME)/scripts/padsc
AST_ARCH := $(shell $(PADS_HOME)/ast-ast/bin/package.cvs)
INSTALLROOT=$(PADS_HOME)/ast-ast/arch/$(AST_ARCH)
LIBDIR=$(INSTALLROOT)/lib
LIBS = $(LIBDIR)/libpads.a $(LIBDIR)/libast.a
INCLUDES = -I$(PADS_HOME)/padsc/include -I$(INSTALLROOT)/include/ast -I.
PADSIDL=$(PADS_HOME)/scratch/padsidl

include $(PADS_HOME)/mk/rules.mk

INCLUDES += -I$(OCAML_LIB_DIR)

# Rule to make programs


padsc.mli padsc.ml padsc_stubs.c:padsc.idl
	camlidl -no-include padsc.idl

pint_c.mli pint_c.ml pint_c_stubs.c:padsc.idl pint_c.idl 
	camlidl -no-include -header pint_c.idl

padsc_stubs_w.o: padsc_stubs.c

pint_c_stubs_w.o: pint_c_stubs.c

%.cmo: %.mli %.ml 
	ocamlc -c $?

.PHONY: clean padsc pint_c

padsc: padsc_stubs_w.o
pint_c: pint_c_stubs_w.o
padsclib: padsc pint_c padsc.cmo pint_c.cmo pint_c.o
	ocamlc -a -o padsc.cma -custom padsc.cmo pint_c.cmo \
	$(PADSIDL)/padsc_stubs_w.o $(PADSIDL)/pint_c_stubs_w.o $(PADSIDL)/pint_c.o \
        $(INSTALLROOT)/lib/libpads.a $(INSTALLROOT)/lib/libast.a $(OCAML_LIB_DIR)/libcamlidl.a

client: padsclib client.ml
	ocamlc client.ml  padsc.cma  -o client

clean:
	$(RM) padsc.ml padsc.mli pint_c.ml pint_c.mli pint_c.h
	$(RM) *.cmo *.cma *.cmi
	$(RM) *_stubs.c *.o
	$(RM) client a.out
