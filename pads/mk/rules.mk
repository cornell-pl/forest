# N.B.: At the top of each Makefile, define the variable PADS_HOME using a
#       relative path, then include this rules.mk file.  Example:
#
#   PADS_HOME = ../..
#   include $(PADS_HOME)/rules.mk
#
# and set VPATH to location(s) of source files.
#
# If you are building the PADSL library, use:
#
#   PADS_HOME = ../..
#   BuildPADSLib = 1
#   include $(PADS_HOME)/rules.mk
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

ifndef AST_ARCH
  AST_ARCH := $(shell $(PADS_HOME)/scripts/package)
  export AST_ARCH
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
STATIC_ASTLIB_NM_D = libast-g.a

STATIC_PADSLIB_NM_O = libpadsc.a
STATIC_PADSLIB_NM_D = libpadsc-g.a

STATIC_COBOL_PADSLIB_NM_O = libpadsc-cobol.a
STATIC_COBOL_PADSLIB_NM_D = libpadsc-cobol-g.a

# SHARED_ASTLIB_NM_O = libast.so.5.4
SHARED_ASTLIB_NM_O = libast.so
SHARED_ASTLIB_NM_D = libast-g.so

SHARED_PADSLIB_NM_O = libpadsc.so.1.0
SHARED_PADSLIB_NM_ALT1_O = libpadsc.so.1
SHARED_PADSLIB_NM_ALT2_O = libpadsc.so

SHARED_PADSLIB_NM_D = libpadsc-g.so.1.0
SHARED_PADSLIB_NM_ALT1_D = libpadsc-g.so.1
SHARED_PADSLIB_NM_ALT2_D = libpadsc-g.so

SHARED_COBOL_PADSLIB_NM_O = libpadsc-cobol.so.1.0
SHARED_COBOL_PADSLIB_NM_ALT1_O = libpadsc-cobol.so.1
SHARED_COBOL_PADSLIB_NM_ALT2_O = libpadsc-cobol.so

SHARED_COBOL_PADSLIB_NM_D = libpadsc-cobol-g.so.1.0
SHARED_COBOL_PADSLIB_NM_ALT1_D = libpadsc-cobol-g.so.1
SHARED_COBOL_PADSLIB_NM_ALT2_D = libpadsc-cobol-g.so

STATIC_LIBTOOL = ar r
STATIC_LIBTOOL_OPTS =
SHARED_LIBTOOL = $(CC) -shared -nostartfiles
SHARED_LIBTOOL_PRE_PADSLIB = -Wl,-whole-archive
SHARED_LIBTOOL_PRE_ASTLIB  = -Wl,-no-whole-archive
SHARED_LIBTOOL_OPTS = -lc
LINKER = $(CC)
LINKOPTS_D = $(CDBGFLAGS) -Wl,-z,origin '-Wl,-R,$$ORIGIN/../lib'
LINKOPTS_O = $(COPTFLAGS) -Wl,-z,origin '-Wl,-R,$$ORIGIN/../lib'
endif

ifeq ($(OPSYS),irix)
CC = /home/gsf/arch/$(AST_ARCH)/bin/cc
CDBGFLAGS = -g -woff 47,1174
COPTFLAGS = -DNDEBUG -O2 -woff 47,1174
ifdef BuildPADSLib
CSHAREFLAGS = -KPIC
else
CSHAREFLAGS =
endif
CARCHFLAGS =
LIB_DEP_PATTERN = %.a
STATIC_ASTLIB_NM = libast-g.a
STATIC_PADSLIB_NM = libpadsc-g.a
STATIC_COBOL_PADSLIB_NM = libpadsc-cobol-g.a
# SHARED_ASTLIB_NM = libast.so.5.4
SHARED_ASTLIB_NM = libast.so
SHARED_PADSLIB_NM = libpadsc.so.1.0
SHARED_PADSLIB_NM_ALT1 = libpadsc.so.1
SHARED_PADSLIB_NM_ALT2 = libpadsc.so
SHARED_COBOL_PADSLIB_NM = libpadsc-cobol.so.1.0
SHARED_COBOL_PADSLIB_NM_ALT1 = libpadsc-cobol.so.1
SHARED_COBOL_PADSLIB_NM_ALT2 = libpadsc-cobol.so
STATIC_LIBTOOL = ar r
STATIC_LIBTOOL_OPTS =
SHARED_LIBTOOL = $(CC) -shared -update_registry $(INSTALLROOT)/lib/registry.ld
SHARED_LIBTOOL_PRE_PADSLIB = -all
SHARED_LIBTOOL_PRE_ASTLIB  = -notall
SHARED_LIBTOOL_OPTS =
LINKER = $(CC)
LINKOPTS_D = $(CDBGFLAGS)
LINKOPTS_O = $(COPTFLAGS)
endif

ifndef SHARED_LIBTOOL_PRE_PADSLIB
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

ASTLIB_DIR = /home/gsf/arch/$(AST_ARCH)/lib

STATIC_PADSLIB_O = $(INSTALLROOT)/lib/$(STATIC_PADSLIB_NM_O)
STATIC_COBOL_PADSLIB_O = $(INSTALLROOT)/lib/$(STATIC_COBOL_PADSLIB_NM_O)
STATIC_ASTLIB_O = $(ASTLIB_DIR)/$(STATIC_ASTLIB_NM_O)
STATIC_LIBS_O = $(STATIC_PADSLIB_O) $(STATIC_ASTLIB_O) 
COBOL_STATIC_LIBS_O = $(STATIC_COBOL_PADSLIB_O) $(STATIC_ASTLIB_O) 
LIB_DEPS_O = $(STATIC_LIBS_O)
COBOL_LIB_DEPS_O = $(COBOL_STATIC_LIBS_O)

DYNAMIC_LIBS_O = -L $(ASTLIB_DIR) -L $(INSTALLROOT)/lib -lpadsc -last
COBOL_DYNAMIC_LIBS_O = -L $(ASTLIB_DIR) -L $(INSTALLROOT)/lib -lpadsc-cobol -last
SHARED_PADSLIB_DEP_O = $(INSTALLROOT)/lib/$(SHARED_PADSLIB_NM_O)
SHARED_COBOL_PADSLIB_DEP_O = $(INSTALLROOT)/lib/$(SHARED_COBOL_PADSLIB_NM_O)
SHARED_ASTLIB_DEP_O = $(ASTLIB_DIR)/$(SHARED_ASTLIB_NM_O)
# DYNAMIC_LIB_DEPS_O = $(SHARED_PADSLIB_DEP_O) $(SHARED_ASTLIB_DEP_O)

STATIC_PADSLIB_D = $(INSTALLROOT)/lib/$(STATIC_PADSLIB_NM_D)
STATIC_COBOL_PADSLIB_D = $(INSTALLROOT)/lib/$(STATIC_COBOL_PADSLIB_NM_D)
STATIC_ASTLIB_D = $(ASTLIB_DIR)/$(STATIC_ASTLIB_NM_D)
STATIC_LIBS_D = $(STATIC_PADSLIB_D) $(STATIC_ASTLIB_D) 
COBOL_STATIC_LIBS_D = $(STATIC_COBOL_PADSLIB_D) $(STATIC_ASTLIB_D) 
LIB_DEPS_D = $(STATIC_LIBS_D)
COBOL_LIB_DEPS_D = $(COBOL_STATIC_LIBS_D)

DYNAMIC_LIBS_D = -L $(ASTLIB_DIR) -L $(INSTALLROOT)/lib -lpadsc-g -last-g
COBOL_DYNAMIC_LIBS_D = -L $(ASTLIB_DIR) -L $(INSTALLROOT)/lib -lpadsc-cobol-g -last-g
SHARED_PADSLIB_DEP_D = $(INSTALLROOT)/lib/$(SHARED_PADSLIB_NM_D)
SHARED_COBOL_PADSLIB_DEP_D = $(INSTALLROOT)/lib/$(SHARED_COBOL_PADSLIB_NM_D)
SHARED_ASTLIB_DEP_D = $(ASTLIB_DIR)/$(SHARED_ASTLIB_NM_D)
# DYNAMIC_LIB_DEPS_D = $(SHARED_PADSLIB_DEP_D) $(SHARED_ASTLIB_DEP_D)

INCLUDES = -I/home/gsf/arch/$(AST_ARCH)/include/ast -I$(PADS_HOME)/include -I/usr/common/lib/ocaml -I. -I..
ifdef GEN_DIR
INCLUDES += -I$(GEN_DIR)
endif
INCLUDE_DEPS = $(PADS_HOME)/include/*.h
DEFS =
ifdef BuildPADSLib
SHAREDEFS = -D_BLD_DLL -D_PACKAGE_ast  # needed?
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

PADSC = $(PADS_HOME)/padsc 
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
ifdef cobol_stuff
cobol_%-g: cobol_%-g.o $(COBOL_LIB_DEPS_D)
	@echo "Using rules.mk rule M_D"
	$(LINK_D) $< $(COBOL_DYNAMIC_LIBS_D) -o $@

cobol_%: cobol_%.o $(COBOL_LIB_DEPS_O)
	@echo "Using rules.mk rule M_O"
	$(LINK_O) $< $(COBOL_DYNAMIC_LIBS_O) -o $@

cobol_%-g.o: cobol_%.c $(INCLUDE_DEPS) $(COBOL_LIB_DEPS_D)
	@echo "Using rules.mk rule N_D"
	$(COMPILE_D) $< $(COBOL_DYNAMIC_LIBS_D) -o $@

cobol_%.o: cobol_%.c $(INCLUDE_DEPS) $(COBOL_LIB_DEPS_O)
	@echo "Using rules.mk rule N_O"
	$(COMPILE_O) $< $(COBOL_DYNAMIC_LIBS_O) -o $@
endif

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
	$(PADSC) $< -r $(GEN_DIR) -I . -I ..
else
$(GEN_DIR)/%.c: %.p $(PADSC) $(PADSC_REAL)
	@echo "Using rule P-nowrite"
	$(PADSC) $< -r $(GEN_DIR) -wnone -I . -I ..
endif
endif

