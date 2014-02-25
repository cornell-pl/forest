 # This is a GNU makefile.

ifndef PADS_HOME
  export PADS_HOME=$(shell cd ../..; pwd)
endif

ifndef AST_ARCH
  export AST_ARCH=$(shell $(PADS_HOME)/ast-ast/bin/package.cvs)
endif

CURDIR := $(shell pwd)
ifneq ($(AST_ARCH),$(notdir $(CURDIR)))
include $(PADS_HOME)/mk/redirect.mk
else

 # The following rules are run from the arch directory

USE_GALAX=1
SCRIPTS = $(PADS_HOME)/scripts
BuildPADSLib = 1
BuildPGLXLib = 1
include $(PADS_HOME)/mk/rules.mk

VPATH = ..

CC=gcc

# Caml object files

pads_c.ml : pads_c.cmi
pads_dm.ml : pads_c.mli
pads_dm.mli : pads_c.mli

CAML_OBJS=pads_c.cmo pads_dm.cmo $(GALAX_SRC)/galapi/c_api/galax_wrap.cmo
CAML_OPTOBJS=$(CAML_OBJS:.cmo=.cmx)
OCAML_FLAGS=-I . -I $(VPATH) -I $(GALAX_HOME)/lib/caml

# C object files
OBJS_O=pads_dm_opt.o $(GALAX_SRC)/galapi/c_api/galax_util.o $(GALAX_SRC)/galapi/c_api/galax_stub.o $(GALAX_SRC)/galapi/c_api/itemlist.o pads_c_stubs.o pads_dm_stub.o pads_pglx.o

# These should all be in a library:
$(GALAX_SRC)/galapi/c_api/galax_util.o :
	echo "Do not recompile $(GALAX_SRC)/galapi/c_api/galax_util.o"
$(GALAX_SRC)/galapi/c_api/galax_stub.o : 
	echo "Do not recompile $(GALAX_SRC)/galapi/c_api/galax_stub.o"
$(GALAX_SRC)/galapi/c_api/itemlist.o : 
	echo "Do not recompile $(GALAX_SRC)/galapi/c_api/itemlist.o"

# No debug options right now
# OBJS_D = $(OBJS_O:%.o=%_d.o)
CHEADERS=pads_c.h pads_dm.h 
INSTALL_INCLUDES=$(CHEADERS) 

# IDL 
# Moving the generated files is a hack!
pads_c.h : pads_c.idl
	$(OCAML_BIN_DIR)/camlidl -header $<
	cp $(VPATH)/pads_c.h  $(VPATH)/pads_c.ml  $(VPATH)/pads_c.mli $(VPATH)/pads_c_stubs.c .

$(VPATH)/pads_c.mli     : pads_c.h
pads_c.ml      : pads_c.h
pads_c_stubs.c : pads_c.h

# Include Galax's own configuration Makefile:
include $(GALAX_HOME)/config/Makefile.galax

# HACK ALERT!
# These definitions are copied from $(GALAX_HOME)/config/Makefile.galax and edited to include -ccopt!

OCAML_CAPI_OPTLIB_PATHS=\
-ccopt -L$(CONF_OCAML_STDLIB) \
-ccopt -L$(CONF_OCAML_LIBPCRE) \
-ccopt -L$(CONF_C_LIBPCRE) \
-ccopt -L$(CONF_C_LIBASMRUN)

OCAML_CAPI_OPTLIBS=\
-ccopt -fno-common \
-ccopt -dynamic \
-ccopt -lnums -ccopt -lunix -ccopt -lstr -ccopt -lbigarray \
-ccopt -lthreadsnat -ccopt -lpthread \
-ccopt -lpcre_stubs \
-ccopt -lpcre 

API_ALL_CAML_LIBS=-linkall $(OCAML_LIB_INCLUDES) $(OCAML_OPTLIBS) $(GALAX_HOME)/lib/caml/galax.cmxa $(OCAML_CAPI_OPTLIB_PATHS) $(OCAML_CAPI_OPTLIBS)

pads_dm_opt.o: $(CAML_OPTOBJS) $(GALAX_HOME)/lib/caml/galax.cmxa
	$(OCAML_BIN_DIR)/ocamlopt -output-obj -o pads_dm_opt.o $(API_ALL_CAML_LIBS) $(CAML_OPTOBJS) 

pads_pglx.o: $(CHEADERS) pads_pglx.c
	$(CC) -fno-common $(CFLAGS_O) -I$(OCAML_LIB_DIR) -I . -I$(GALAXHOME)/lib/c -I$(OCAMLHOME)/caml  -I $(PADS_HOME)/padsc/include -c $(VPATH)/pads_pglx.c

all : $(STATIC_GALAXLIB_O) $(SHARED_GALAXLIB_O) $(STATIC_GALAXLIB_D)
	echo "Making all in pads-glx"

opt : $(STATIC_GALAXLIB_O) $(SHARED_GALAXLIB_O)
	echo "Making opt in pads-glx"

debug : $(STATIC_GALAXLIB_D)
	echo "Making debug in pads-glx"

$(STATIC_GALAXLIB_O) : $(OBJS_O)
	$(STATIC_LIBTOOL) $(STATIC_GALAXLIB_O) $(STATIC_LIBTOOL_OPTS) $(OBJS_O) $(OCAML_LIB_DIR)/std-lib/libasmrun.a $(OCAML_LIB_DIR)/std-lib/libcamlidl.a
	(lib=$@; $(FixStaticLib))

$(SHARED_GALAXLIB_O) : $(OBJS_O)
	$(SHARED_LIBTOOL) -o $(SHARED_GALAXLIB_O) $(SHARED_LIBTOOL_WHOLE_ARCHIVE) \
        $(OBJS_O) $(SHARED_LIBTOOL_NOT_WHOLE_ARCHIVE) $(SHARED_LIBTOOL_OPTS) $(OCAML_LIB_DIR)/std-lib/libasmrun.a $(OCAML_LIB_DIR)/std-lib/libcamlidl.a
#        $(OBJS_O) $(SHARED_LIBTOOL_NOT_WHOLE_ARCHIVE) $(SHARED_LIBTOOL_OPTS) $(OCAML_LIB_DIR)/libasmrun.a $(OCAML_LIB_DIR)/libcamlidl.a
$(STATIC_GALAXLIB_D) : $(OBJS_O)
	$(STATIC_LIBTOOL) $(STATIC_GALAXLIB_D) $(STATIC_LIBTOOL_OPTS) $(OBJS_O) $(OCAML_LIB_DIR)/std-lib/libasmrun.a $(OCAML_LIB_DIR)/std-lib/libcamlidl.a
	(lib=$@; $(FixStaticLib))

install : all
	+@(set -x ; \
	$(PADS_HOME)/scripts/install-sh -c -m 444 $(STATIC_GALAXLIB_O) $(INSTALL_LIBDIR); \
	(lib=$(INSTALL_LIBDIR)/$(STATIC_GALAXLIB_O); $(FixStaticLib)); \
	$(PADS_HOME)/scripts/install-sh -c -m 555 $(SHARED_GALAXLIB_O) $(INSTALL_LIBDIR); \
	$(PADS_HOME)/scripts/install-sh -c -m 444 $(STATIC_GALAXLIB_D) $(INSTALL_LIBDIR); \
	(lib=$(INSTALL_LIBDIR)/$(STATIC_GALAXLIB_D); $(FixStaticLib)); \
	for file in $(INSTALL_INCLUDES); do \
		$(PADS_HOME)/scripts/install-sh -c -m 444 $(VPATH)/$$file $(INSTALL_INCDIR); \
	done;)

depend: pads_c.h
	$(OCAML_BIN_DIR)/ocamldep $(OCAML_FLAGS) -I $(VPATH) $(VPATH)/*.mli $(VPATH)/*.ml >$(VPATH)/.depend

clean : 
	$(RM) $(STATIC_GALAXLIB_O) $(STATIC_GALAXLIB_D) $(SHARED_GALAXLIB_O)  *~ *.o *.cm* ../*~ \
	$(VPATH)/*.cm* pads_c.ml* pads_c_stubs.c pads_c.h $(VPATH)/pads_c.ml* $(VPATH)/pads_c_stubs.c $(VPATH)/pads_c.h 

veryclean : clean

 # End of rules that are run from the arch directory
endif

.SUFFIXES: .ml .mli .mll .cmo .cmi .cmx

pads_dm.cmi: ../pads_dm.mli pads_c.cmi
	$(OCAML_BIN_DIR)/ocamlc  -o $*.cmi $(OCAML_FLAGS) -c $(VPATH)/pads_dm.mli

pads_dm.cmo: pads_dm.cmi $(VPATH)/pads_dm.ml
	$(OCAML_BIN_DIR)/ocamlc  -o $*.cmo $(OCAML_FLAGS) -c $(VPATH)/pads_dm.ml

pads_dm.cmx: pads_dm.cmi $(VPATH)/pads_dm.ml
	$(OCAML_BIN_DIR)/ocamlopt  -o $*.cmx $(OCAML_FLAGS) -c $(VPATH)/pads_dm.ml

pads_c.cmi: $(VPATH)/pads_c.mli pads_c.mli
	$(OCAML_BIN_DIR)/ocamlc  -o pads_c.cmi $(OCAML_FLAGS) -c $(VPATH)/pads_c.mli

pads_c.cmx: pads_c.cmi pads_c.ml 
	$(OCAML_BIN_DIR)/ocamlopt  -o pads_c.cmx $(OCAML_FLAGS) -c $(VPATH)/pads_c.ml

pads_c.cmo: pads_c.cmi pads_c.ml 
	$(OCAML_BIN_DIR)/ocamlc  -o pads_c.cmo $(OCAML_FLAGS) -c $(VPATH)/pads_c.ml

.c.o: $(CHEADERS)
	$(CC) -fno-common $(CFLAGS) -I . -I$(GALAX_HOME)/lib/c -c $(VPATH)/$<
	cp $*.o .

# Dependencies
#include $(VPATH)/.depend
