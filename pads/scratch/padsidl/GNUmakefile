 # This makefile uses gmake extensions
 # such as $(shell ...) to execute a command

.PRECIOUS: %.c

# These variables need to be set
ifndef PADS_HOME
  $(error Need to set shell variable PADS_HOME)
endif

# These variables should not need to change
PADSC=$(PADS_HOME)/scripts/padsc
AST_ARCH := $(shell $(PADS_HOME)/ast-ast/bin/package.cvs)
INSTALLROOT=$(PADS_HOME)/ast-ast/arch/$(AST_ARCH)
LIBDIR=$(INSTALLROOT)/lib
LIBS = $(LIBDIR)/libpads.a $(LIBDIR)/libast.a
INCLUDES = -I$(PADS_HOME)/padsc/include -I$(INSTALLROOT)/include/ast -I.
PADSIDL=$(PADS_HOME)/scratch/padsidl

BASE_TYPES=pint pchar pstring
GENERATED_INTS=$(foreach base_type,$(BASE_TYPES),$(base_type)_c.mli)
GENERATED_IMPLS=$(foreach base_type,$(BASE_TYPES),$(base_type)_c.ml)
GENERATED_STUBS=$(foreach base_type,$(BASE_TYPES),$(base_type)_c_stubs.c)
BT_GEN=$(GENERATED_INTS) $(GENERATED_IMPLS) $(GENERATED_STUBS)
WRAPPERS=$(foreach base_type,$(BASE_TYPES),$(base_type)_c_stubs_w.c)
BT_CAML_OBJS=$(foreach base_type,$(BASE_TYPES),$(base_type)_c.cmo)
BT_C_OBJS=$(foreach base_type,$(BASE_TYPES),$(base_type)_c_stubs_w.o $(base_type)_c.o)
BT_OBJS=$(BT_CAML_OBJS) $(BT_C_OBJS)

PADSC_CAML_OBJS=padsc.cmo
PADSC_C_OBJS=padsc_stubs_w.o
PADSC_OBJS=$(PADSC_CAML_OBJS) $(PADSC_C_OBJS)

CAML_OBJS=$(PADSC_CAML_OBJS) $(BT_CAML_OBJS)
C_OBJS=$(PADSC_C_OBJS) $(BT_C_OBJS)
OBJS=$(PADSC_OBJS) $(BT_OBJS)

# Name of padsidl library
PADSIDL_NAME=padsidl
PADSIDL_LIB_NM=$(mam_cc_PREFIX_ARCHIVE)$(PADSIDL_NAME)$(mam_cc_SUFFIX_ARCHIVE)
# Fully qualified names of object files.  Need fully qualifed file
# names for C object files so that they can be found by OCaml programs linking to
# the library.  The key point is that only their names are included in
# the OCaml lib, not the actual object files.
Q_OBJS=$(foreach obj,$(OBJS),$(PADSIDL)/$(obj))

include $(PADS_HOME)/mk/rules.mk

INCLUDES += -I$(OCAML_LIB_DIR)

# Rule to make programs

padsc.mli padsc.ml padsc_stubs.c:padsc.idl
	camlidl -no-include padsc.idl

$(GENERATED_INTS):%.mli: %.idl
	camlidl -no-include -header $*.idl

$(GENERATED_IMPLS):%.ml: %.idl
	camlidl -no-include -header $*.idl

$(GENERATED_STUBS):%_stubs.c: %.idl
	camlidl -no-include -header $*.idl

padsc_stubs_w.c: padsc_stubs.c

$(WRAPPERS):%_stubs_w.c: %_stubs.c

$(CAML_OBJS): %.cmo: %.mli %.ml 
	ocamlc -c $^

%.cmo: %.mli %.ml 
	ocamlc -c $^

$(PADSIDL_LIB_NM) : $(C_OBJS)
	$(STATIC_LIBTOOL) $(PADSIDL_LIB_NM) $(STATIC_LIBTOOL_OPTS) $^
	(lib=$@; $(FixStaticLib))

$(PADSIDL)/padsc.cma padsc.cma: $(CAML_OBJS) $(PADSIDL_LIB_NM)
	ocamlc -a -o padsc.cma -custom \
	-ccopt -L$(PADSIDL) -cclib -l$(PADSIDL_NAME) \
        -ccopt -L$(INSTALLROOT)/lib -cclib -lpads -cclib -last -ccopt -L$(OCAML_LIB_DIR) \
	-cclib -lcamlidl
# 	ocamlc -a -o padsc.cma -custom $(Q_OBJS)\
#         $(INSTALLROOT)/lib/libpads.a $(INSTALLROOT)/lib/libast.a $(OCAML_LIB_DIR)/libcamlidl.a

client: padsclib client.ml
	ocamlc client.ml  padsc.cma  -o client

.PHONY: clean padsc padsclib

padsc: padsc_stubs_w.o

padsclib: padsc.cma

clean:
	$(RM) padsc.ml padsc.mli 
	$(RM) $(BT_GEN)
	$(RM) *.cmo *.cma *.cmi
	$(RM) *.o
	$(RM) client a.out
