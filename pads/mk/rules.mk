# N.B.: At the top of each Makefile, define the variable PADS_HOME using a
#       relative path, then include this rules.mk file.  Example:
#
#   PADS_HOME = ../../..
#   include $(PADS_HOME)/mk/rules.mk
#
# and set VPATH to location(s) of source files.
#
# If you are building the PADSL library, use:
#
#   PADS_HOME = ../../..
#   BuildPADSLib = 1
#   include $(PADS_HOME)/mk/rules.mk
#
# and set VPATH to location(s) of source files.
#
# If there is a directory with generated .h/.c files, define GEN_DIR before including rules.mk
#
# Static link rule:
# foo : foo.o $(LIB_DEPS_O)
# 	$(LINK_O) -o $< foo.o $(STATIC_LIBS_O)

# Shared lib link:
# foo : foo.o $(LIB_DEPS_O)
# 	$(LINK_O) -o $< foo.o $(DYNAMIC_LIBS_O)
#
# (You can use _D instead of _O to choose debug rather than optimized)
#
# If using rules that cause the padsc compiler to be called, you can
# define PADSC_EXTRA to specify extra padsc params (such as -x).
#
# If GEN_GALAX is defined, the padsc option -x is included
# and the libpglx library is added to the set of libraries to link against.
#
# If USE_GALAX is defined, the GALAX include paths are added as -I
# options and the appropriate ocaml and Galax libraries are added
# to the set of libraries to link against. In addition, the compilation
# flag -DUSE_GALAX is added.
#
# If USE_GALAX is NOT defined, then there is no -DUSE_GALAX flag, and
# this will cause 'fake' versions of the following to be added to
# libglx:
#   failwith
#   glx_atomicString
#   glx_atomicBoolean
#   glx_atomicInt
#   glx_atomicInteger
#   glx_atomicDecimal
#   glx_atomicFloat
#   glx_atomicDouble
#   glx_atomicAnyURI
#   glx_string_of_atomicValue
#
# Thus, GEN_GALAX without USE_GALAX allows you to test C code against the
# fake versions of the above, while GEN_GALAX plus USE_GALAX is for
# real testing.
#
# (IF USE_GALAX is defined, GEN_GALAX is automatically defined)
#

ifndef AST_ARCH
AST_ARCH := $(shell $(PADS_HOME)/ast-base/bin/package)
export AST_ARCH
endif

ifndef AST_HOME
AST_HOME := $(PADS_HOME)/ast-base/arch/$(AST_ARCH)
export AST_HOME
endif

ifndef GALAX_HOME
GALAX_HOME := /home/mff/Galax-rh7
export GALAX_HOME
endif
GALAX_LIB_DIR = $(GALAX_HOME)/lib/c

ifndef OCAML_LIB_DIR
OCAML_LIB_DIR = /usr/common/lib/ocaml
export OCAML_LIB_DIR
endif

ifndef USR_LIB_DIR
USER_LIB_DIR = /usr/lib
endif

ifdef USE_GALAX
GEN_GALAX = 1
endif

ARCH_N_OPSYS = $(shell $(PADS_HOME)/scripts/arch-n-opsys)
OPSYS = $(shell $(PADS_HOME)/scripts/opsys)

LIB_DEP_PATTERN = %.a

STATIC_ASTLIB_NM_O = libast.a
STATIC_ASTLIB_NM_D = libast.a

STATIC_PADSLIB_NM_O = libpadsc.a
STATIC_PADSLIB_NM_D = libpadsc-g.a

STATIC_PGLXLIB_NM_O = libpglx.a
STATIC_PGLXLIB_NM_D = libpglx-g.a

SHARED_ASTLIB_NM_O = libast.so
SHARED_ASTLIB_NM_D = libast.so

SHARED_PADSLIB_NM_O = libpadsc.so.1.0
SHARED_PADSLIB_NM_ALT1_O = libpadsc.so.1
SHARED_PADSLIB_NM_ALT2_O = libpadsc.so

SHARED_PADSLIB_NM_D = libpadsc-g.so.1.0
SHARED_PADSLIB_NM_ALT1_D = libpadsc-g.so.1
SHARED_PADSLIB_NM_ALT2_D = libpadsc-g.so

SHARED_PGLXLIB_NM_O = libpglxc.so.1.0
SHARED_PGLXLIB_NM_ALT1_O = libpglxc.so.1
SHARED_PGLXLIB_NM_ALT2_O = libpglxc.so

SHARED_PGLXLIB_NM_D = libpglxc-g.so.1.0
SHARED_PGLXLIB_NM_ALT1_D = libpglxc-g.so.1
SHARED_PGLXLIB_NM_ALT2_D = libpglxc-g.so

# OS specific rules
# (may override some of the above)

ifeq ($(OPSYS),linux)
ifndef CC
CC = gcc
endif
CDBGFLAGS = -g -Wall
COPTFLAGS = -Wall -DNDEBUG -O2
ifdef BuildPADSLib
CSHAREFLAGS = -fpic
else
CSHAREFLAGS =
endif
CARCHFLAGS =
STATIC_LIBTOOL = ar r
STATIC_LIBTOOL_OPTS =
SHARED_LIBTOOL = $(CC) -shared -nostartfiles
SHARED_LIBTOOL_WHOLE_ARCHIVE = -Wl,-whole-archive
SHARED_LIBTOOL_NOT_WHOLE_ARCHIVE = -Wl,-no-whole-archive
SHARED_LIBTOOL_OPTS = -lc
LINKER = $(CC)
LINKOPTS_D = $(CDBGFLAGS) -Wl,-z,origin '-Wl,-R,$$ORIGIN/../lib'
LINKOPTS_O = $(COPTFLAGS) -Wl,-z,origin '-Wl,-R,$$ORIGIN/../lib'
endif

ifeq ($(OPSYS),irix)
ifndef CC
CC = cc
endif
CDBGFLAGS = -g -woff 47,1174
COPTFLAGS = -DNDEBUG -O2 -woff 47,1174
ifdef BuildPADSLib
CSHAREFLAGS = -KPIC
else
CSHAREFLAGS =
endif
CARCHFLAGS =
STATIC_LIBTOOL = ar r
STATIC_LIBTOOL_OPTS =
SHARED_LIBTOOL = $(CC) -shared -update_registry $(LIB_DIR)/registry.ld
SHARED_LIBTOOL_WHOLE_ARCHIVE = -all
SHARED_LIBTOOL_NOT_WHOLE_ARCHIVE = -notall
SHARED_LIBTOOL_OPTS =
LINKER = $(CC)
LINKOPTS_D = $(CDBGFLAGS)
LINKOPTS_O = $(COPTFLAGS)
endif

ifndef SHARED_LIBTOOL_WHOLE_ARCHIVE
%: forceabort1
	@echo "ERROR: did not recognize operating system $(OPSYS)"
	@exit 1
forceabort1: ;
endif

# Done with architecture-specific stuff

ifdef GEN_GALAX
PADSC_EXTRA = -x
endif

ifdef USE_GALAX
CDBGFLAGS += -DUSE_GALAX
COPTFLAGS += -DUSE_GALAX
endif

ifndef INSTALLROOT
%: forceabort2
	@echo "ERROR: env variable INSTALLROOT must be defined"
	@exit 1
forceabort2: ;
endif

LIB_DIR = $(INSTALLROOT)/lib

STATIC_PADSLIB_O = $(LIB_DIR)/$(STATIC_PADSLIB_NM_O)
STATIC_PGLXLIB_O = $(LIB_DIR)/$(STATIC_PGLXLIB_NM_O)
STATIC_ASTLIB_O = $(LIB_DIR)/$(STATIC_ASTLIB_NM_O)
# mff may need to change next two defns
STATIC_GALAXLIB_O = $(GALAX_LIB_DIR)/libglxopt.a
STATIC_OCAMLLIB_O = \
  $(OCAML_LIB_DIR)/libnums.a \
  $(USER_LIB_DIR)/libm.a \
  $(USER_LIB_DIR)/libdl.a \
  $(USER_LIB_DIR)/libcurses.a \
  $(OCAML_LIB_DIR)/libunix.a \
  $(OCAML_LIB_DIR)/libstr.a
# XXX what about libcamlrun.a ?
ifdef GEN_GALAX
STATIC_LIBS_O = $(STATIC_PADSLIB_O) $(STATIC_PGLXLIB_O) $(STATIC_ASTLIB_O) 
else
STATIC_LIBS_O = $(STATIC_PADSLIB_O) $(STATIC_ASTLIB_O) 
endif
ifdef USE_GALAX
STATIC_LIBS_O += $(STATIC_GALAXLIB_O) $(STATIC_OCAMLLIB_O)
endif
LIB_DEPS_O = $(STATIC_LIBS_O)

STATIC_PADSLIB_D = $(LIB_DIR)/$(STATIC_PADSLIB_NM_D)
STATIC_PGLXLIB_D = $(LIB_DIR)/$(STATIC_PGLXLIB_NM_D)
STATIC_ASTLIB_D = $(LIB_DIR)/$(STATIC_ASTLIB_NM_D)
# mff may need to change next two defns
STATIC_GALAXLIB_D = $(GALAX_LIB_DIR)/libglxopt.a
STATIC_OCAMLLIB_D = $(STATIC_OCAMLLIB_O) # no debug versions available
ifdef GEN_GALAX
STATIC_LIBS_D = $(STATIC_PADSLIB_D) $(STATIC_PGLXLIB_D) $(STATIC_ASTLIB_D) 
else
STATIC_LIBS_D = $(STATIC_PADSLIB_D) $(STATIC_ASTLIB_D) 
endif
ifdef USE_GALAX
STATIC_LIBS_D += $(STATIC_GALAXLIB_D) $(STATIC_OCAMLLIB_D)
endif
LIB_DEPS_D = $(STATIC_LIBS_D)

ifdef GEN_GALAX
DYNAMIC_LIBS_O = -L $(LIB_DIR) -lpadsc -lpglx -last 
else
DYNAMIC_LIBS_O = -L $(LIB_DIR) -lpadsc -last
endif
ifdef USE_GALAX
# mff may need to change next line
DYNAMIC_LIBS_O += -L $(GALAX_LIB_DIR) -lglxopt -L $(OCAML_LIB_DIR) -lnums -lm -ldl -lcurses -lunix -lstr
endif
SHARED_PADSLIB_DEP_O = $(LIB_DIR)/$(SHARED_PADSLIB_NM_O)
SHARED_PGLXLIB_DEP_O = $(LIB_DIR)/$(SHARED_PGLXLIB_NM_O)
SHARED_ASTLIB_DEP_O = $(LIB_DIR)/$(SHARED_ASTLIB_NM_O)
ifdef GEN_GALAX
DYNAMIC_LIB_DEPS_O = $(SHARED_PADSLIB_DEP_O) $(SHARED_PGLXLIB_DEP_O) $(SHARED_ASTLIB_DEP_O)
else
DYNAMIC_LIB_DEPS_O = $(SHARED_PADSLIB_DEP_O) $(SHARED_ASTLIB_DEP_O)
endif
ifdef USE_GALAX
# only statics available
DYNAMIC_LIB_DEPS_O += $(STATIC_GALAXLIB_O) $(STATIC_OCAMLLIB_O)
endif

ifdef GEN_GALAX
DYNAMIC_LIBS_D = -L $(LIB_DIR) -lpadsc-g -lpglx-g -last
else
DYNAMIC_LIBS_D = -L $(LIB_DIR) -lpadsc-g -last
endif
ifdef USE_GALAX
# mff may need to change next line 
DYNAMIC_LIBS_D += -L $(GALAX_LIB_DIR) -lglxopt -L $(OCAML_LIB_DIR) -lnums -lm -ldl -lcurses -lunix -lstr
# XXX what about -lcamlrun ?
endif
SHARED_PADSLIB_DEP_D = $(LIB_DIR)/$(SHARED_PADSLIB_NM_D)
SHARED_PGLXLIB_DEP_D = $(LIB_DIR)/$(SHARED_PGLXLIB_NM_D)
SHARED_ASTLIB_DEP_D = $(LIB_DIR)/$(SHARED_ASTLIB_NM_D)
ifdef GEN_GALAX
DYNAMIC_LIB_DEPS_D = $(SHARED_PADSLIB_DEP_D) $(SHARED_PGLXLIB_DEP_D) $(SHARED_ASTLIB_DEP_D)
else
DYNAMIC_LIB_DEPS_D = $(SHARED_PADSLIB_DEP_D) $(SHARED_ASTLIB_DEP_D)
endif
ifdef USE_GALAX
# only statics available
DYNAMIC_LIB_DEPS_D += $(STATIC_GALAXLIB_D) $(STATIC_OCAMLLIB_D)
endif

INCLUDES =  -I. -I.. -I$(AST_HOME)/include/ast
ifdef GEN_DIR
INCLUDES += -I$(GEN_DIR)
endif

ifdef USE_GALAX
INCLUDES +=  -I$(GALAX_LIB_DIR) -I$(OCAML_LIB_DIR)
endif

ifndef BuildAST4PADSLib
INCLUDES += -I$(PADS_HOME)/padsc/include
INCLUDE_DEPS = $(PADS_HOME)/padsc/include/*.h
else
INCLUDE_DEPS =
endif
DEFS =
ifdef BuildPADSLib
SHAREDEFS = -D_BLD_DLL -D_PACKAGE_ast
# SHAREDEFS = -D_BLD_DLL
else
SHAREDEFS =
endif

CFLAGS_D = $(CDBGFLAGS) $(CARCHFLAGS) $(INCLUDES)
COMPILE_D = $(CC) $(CSHAREFLAGS) $(CFLAGS_D) $(DEFS) $(SHAREDEFS)
MKSRC_D = $(CC) -E $(CFLAGS_D)

CFLAGS_O = $(COPTFLAGS) $(CARCHFLAGS) $(INCLUDES)
COMPILE_O = $(CC) $(CSHAREFLAGS) $(CFLAGS_O) $(DEFS) $(SHAREDEFS)
MKSRC_O = $(CC) -E $(CFLAGS_O)

LINK_D = $(LINKER) $(LINKOPTS_D)
LINK_O = $(LINKER) $(LINKOPTS_O)

PADSC = $(PADS_HOME)/scripts/padsc 
PADSC_REAL = $(PADS_HOME)/lib/padsc.$(ARCH_N_OPSYS)

define SanityCheck
( if [ ! -e $(PADSC) ]; then \
      echo "\nUNEXPECTED: $(PADSC) not found\n"; \
      exit 1; \
  fi; \
  if [ ! -e $(PADSC_REAL) ]; then \
      echo "\nUNEXPECTED: padsc compiler obj $(PADSC_REAL) not found"; \
      echo "     Have you built the PADS compiler?\n     Try: using 'gmake' in the top-level padsc directory.\n"; \
      exit 1; \
  fi; \
  for file in $(LIB_DEPS_D) $(LIB_DEPS_O); do \
    if [ ! -e $$file ]; then \
      echo "UNEXPECTED: library $$file does not exist"; \
      exit 1; \
    fi; \
  done; \
)
endef

define LibSanityCheck
( for file in $(LIB_DEPS_D) $(LIB_DEPS_O); do \
    if [ ! -e $$file ]; then \
      echo "UNEXPECTED: library $$file does not exist"; \
      exit 1; \
    fi; \
  done; \
)
endef

define CCExec_DYNAMIC_D
(set -x; \
 $(RM) $@; \
 $(COMPILE_D) $(patsubst $(LIB_DEP_PATTERN),,$(patsubst %.h,,$^)) $(DYNAMIC_LIBS_D) -o $@; \
)
endef

define CCExec_DYNAMIC_O
(set -x; \
 $(RM) $@; \
 $(COMPILE_O) $(patsubst $(LIB_DEP_PATTERN),,$(patsubst %.h,,$^)) $(DYNAMIC_LIBS_O) -o $@; \
)
endef

define CCExec_STATIC_D
(set -x; \
 $(RM) $@; \
 $(COMPILE_D) $(patsubst $(LIB_DEP_PATTERN),,$(patsubst %.h,,$^)) $(STATIC_LIBS_D) -o $@; \
)
endef

define CCExec_STATIC_O
(set -x; \
 $(RM) $@; \
 $(COMPILE_O) $(patsubst $(LIB_DEP_PATTERN),,$(patsubst %.h,,$^)) $(STATIC_LIBS_O) -o $@; \
)
endef

define RegressPre
(echo " "; echo "Performing $@"; \
  if [ -e tmp ]; then echo -n "";else mkdir tmp; fi; \
  $(RM) tmp/tmp.$<; \
)
endef

define RegressPost
(echo diff tmp/tmp.$< ../../regress/$<.regress; diff tmp/tmp.$< ../../regress/$<.regress || echo "**********" $< DIFFERS; \
  echo " "; )
endef

define RegressDef
(echo " "; echo "Performing $@"; \
  if [ -e tmp ]; then echo -n "";else mkdir tmp; fi; \
  $(RM) tmp/tmp.$<$$suf; \
  regfile=`echo ../../regress/$<.regress$$suf | sed -e 's|-g||'`; \
  echo "(./$< $$args 2>&1) | $(PADS_HOME)/scripts/remove_junk.pl | cat > tmp/tmp.$<$$suf"; \
  (./$< $$args 2>&1) | $(PADS_HOME)/scripts/remove_junk.pl | cat > tmp/tmp.$<$$suf; \
  echo diff tmp/tmp.$<$$suf $$regfile; diff tmp/tmp.$<$$suf $$regfile || echo "**********" $<$$suf DIFFERS; \
  echo " "; )
endef

define RegressInput
(echo " "; echo "Performing $@"; \
  if [ -e tmp ]; then echo -n "";else mkdir tmp; fi; \
  $(RM) tmp/tmp.$<$$suf; \
  regfile=`echo ../../regress/$<.regress$$suf | sed -e 's|-g||'`; \
  echo "(./$< $$args < $$input 2>&1) | $(PADS_HOME)/scripts/remove_junk.pl | cat > tmp/tmp.$<$$suf"; \
  (./$< $$args < $$input 2>&1) | $(PADS_HOME)/scripts/remove_junk.pl | cat > tmp/tmp.$<$$suf; \
  echo diff tmp/tmp.$<$$suf $$regfile; diff tmp/tmp.$<$$suf $$regfile || echo "**********" $<$$suf DIFFERS; \
  echo " "; )
endef

define RegressFilter
(echo " "; echo "Performing $@"; \
  if [ -e tmp ]; then echo -n "";else mkdir tmp; fi; \
  $(RM) tmp/tmp.$<$$suf; \
  regfile=`echo ../../regress/$<.regress$$suf | sed -e 's|-g||'`; \
  echo "(./$< $$args 2>&1) | $(PADS_HOME)/scripts/remove_junk.pl | grep $$filter | cat > tmp/tmp.$<$$suf"; \
  (./$< $$args 2>&1) | $(PADS_HOME)/scripts/remove_junk.pl | grep $$filter | cat > tmp/tmp.$<$$suf; \
  echo diff tmp/tmp.$<$$suf $$regfile; diff tmp/tmp.$<$$suf $$regfile || echo "**********" $<$$suf DIFFERS; \
  echo " "; )
endef

.SUFFIXES:
.SUFFIXES: .c .o

ifdef BuildPADSLib
%-g.o: %.c $(INCLUDE_DEPS_ADD) $(INCLUDE_DEPS)
	@echo "Using rules.mk rule A_D"
	$(COMPILE_D) -c $< -o $@

%.o: %.c $(INCLUDE_DEPS_ADD) $(INCLUDE_DEPS)
	@echo "Using rules.mk rule A_O"
	$(COMPILE_O) -c $< -o $@
else

%-g: %-g.o $(LIB_DEPS_D)
	@echo "Using rules.mk rule J_D"
	$(LINK_D) $< $(DYNAMIC_LIBS_D) -o $@

%: %.o $(LIB_DEPS_O)
	@echo "Using rules.mk rule J_O"
	$(LINK_O) $< $(DYNAMIC_LIBS_O) -o $@

%-g.o: %.c $(INCLUDE_DEPS)
	@echo "Using rules.mk rule K_D"
	$(COMPILE_D) -c $< -o $@

%.o: %.c $(INCLUDE_DEPS)
	@echo "Using rules.mk rule K_O"
	$(COMPILE_O) -c $< -o $@

%-g: %.c $(INCLUDE_DEPS) $(LIB_DEPS_D)
	@echo "Using rules.mk rule L_D"
	$(COMPILE_D) $< $(DYNAMIC_LIBS_D) -o $@

%: %.c $(INCLUDE_DEPS) $(LIB_DEPS_O)
	@echo "Using rules.mk rule L_O"
	$(COMPILE_O) $< $(DYNAMIC_LIBS_O) -o $@

endif

ifdef GEN_DIR
ifdef GEN_WRITE
$(GEN_DIR)/%.c: %.p $(PADSC) $(PADSC_REAL)
	@echo "Using rule P"
	$(PADSC) $< $(PADSC_EXTRA) -r $(GEN_DIR) -I. -I..
else
$(GEN_DIR)/%.c: %.p $(PADSC) $(PADSC_REAL)
	@echo "Using rule P-nowrite"
	$(PADSC) $< $(PADSC_EXTRA) -r $(GEN_DIR) -wnone -I. -I..
endif
endif

