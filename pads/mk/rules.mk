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

ifndef AST_ARCH
  AST_ARCH := $(shell $(PADS_HOME)/ast-base/bin/package)
  export AST_ARCH
endif

ifndef AST_HOME
  AST_HOME := $(PADS_HOME)/ast-base/arch/$(AST_ARCH)
  export AST_HOME
endif

ARCH_N_OPSYS = $(shell $(PADS_HOME)/scripts/arch-n-opsys)
OPSYS = $(shell $(PADS_HOME)/scripts/opsys)

# OS specific rules

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
CC = cc
CDBGFLAGS = -g -woff 47,1174
COPTFLAGS = -DNDEBUG -O2 -woff 47,1174
ifdef BuildPADSLib
CSHAREFLAGS = -KPIC
else
CSHAREFLAGS =
endif
CARCHFLAGS =
LIB_DEP_PATTERN = %.a
STATIC_ASTLIB_NM = libast.a
STATIC_PADSLIB_NM = libpadsc-g.a
SHARED_ASTLIB_NM = libast.so
SHARED_PADSLIB_NM = libpadsc.so.1.0
SHARED_PADSLIB_NM_ALT1 = libpadsc.so.1
SHARED_PADSLIB_NM_ALT2 = libpadsc.so
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


# Common rules

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
STATIC_LIBS_O = $(STATIC_PADSLIB_O) $(STATIC_ASTLIB_O) 
LIB_DEPS_O = $(STATIC_LIBS_O)

DYNAMIC_LIBS_O = -L $(LIB_DIR) -lpadsc -lpglx -last
SHARED_PADSLIB_DEP_O = $(LIB_DIR)/$(SHARED_PADSLIB_NM_O)
SHARED_ASTLIB_DEP_O = $(LIB_DIR)/$(SHARED_ASTLIB_NM_O)
# DYNAMIC_LIB_DEPS_O = $(SHARED_PADSLIB_DEP_O) $(SHARED_ASTLIB_DEP_O)

STATIC_PADSLIB_D = $(LIB_DIR)/$(STATIC_PADSLIB_NM_D)
STATIC_PGLXLIB_D = $(LIB_DIR)/$(STATIC_PGLXLIB_NM_D)
STATIC_ASTLIB_D = $(LIB_DIR)/$(STATIC_ASTLIB_NM_D)
STATIC_LIBS_D = $(STATIC_PADSLIB_D) $(STATIC_ASTLIB_D) 
LIB_DEPS_D = $(STATIC_LIBS_D)

DYNAMIC_LIBS_D = -L $(LIB_DIR) -lpadsc-g -lpglx-g -last
SHARED_PADSLIB_DEP_D = $(LIB_DIR)/$(SHARED_PADSLIB_NM_D)
SHARED_ASTLIB_DEP_D = $(LIB_DIR)/$(SHARED_ASTLIB_NM_D)
# DYNAMIC_LIB_DEPS_D = $(SHARED_PADSLIB_DEP_D) $(SHARED_ASTLIB_DEP_D)

INCLUDES =  -I. -I.. -I$(AST_HOME)/include/ast
ifdef GEN_DIR
INCLUDES += -I$(GEN_DIR)
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

ifdef GEN_GALAX
INCLUDES +=  -I /usr/common/lib/ocaml
DYNAMIC_LIBS_O += -lpglx -last
DYNAMIC_LIBS_D += -lpglx-g -last

STATIC_LIBS_O += $(STATIC_PGLXLIB_O)
STATIC_LIBS_D += $(STATIC_PGLXLIB_D)

DYNAMIC_OCAML_LIBS_D = -L/home/mff/Galax-0.3.linux/lib/c -lglx -lnums -lm -ldl -lcurses -lunix -lstr
DYNAMIC_OCAML_LIBS_O = $(DYNAMIC_OCAML_LIBS_D)
ifdef USE_GALAX
DYNAMIC_LIBS_D += $(DYNAMIC_OCAML_LIBS_D)
DYNAMIC_LIBS_O += $(DYNAMIC_OCAML_LIBS_O)
endif

STATIC_OCAML_LIBS_D = /usr/common/lib/ocaml/libcamlrun.a /usr/common/lib/ocaml/libunix.a /usr/common/lib/ocaml/libstr.a /usr/common/lib/ocaml/libnums.a
STATIC_OCAML_LIBS_O = $(STATIC_OCAML_LIBS_D)
ifdef USE_GALAX
STATIC_LIBS_D += $(STATIC_OCAML_LIBS_D)
STATIC_LIBS_O += $(STATIC_OCAML_LIBS_O)
endif
endif

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

ifdef BuildPGLXLib
# location of ocaml/C include files: 
INCLUDES += -I/usr/common/lib/ocaml
endif

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
	$(PADSC) $< $(PADSC_EXTRA) -r $(GEN_DIR) -I . -I ..
else
$(GEN_DIR)/%.c: %.p $(PADSC) $(PADSC_REAL)
	@echo "Using rule P-nowrite"
	$(PADSC) $< $(PADSC_EXTRA) -r $(GEN_DIR) -wnone -I . -I ..
endif
endif

