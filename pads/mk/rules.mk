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
# foo : foo.o $(LIB_DEPS)
# 	$(LINK) -o $< foo.o $(STATIC_LIBS)

# Shared lib link:
# foo : foo.o $(LIB_DEPS)
# 	$(LINK) -o $< foo.o $(DYNAMIC_LIBS)

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
ifdef BuildPADSLib
CSHAREFLAGS = -fpic
else
CSHAREFLAGS =
endif
CARCHFLAGS =
LIB_DEP_PATTERN = %.a
STATIC_ASTLIB_NM = libast-g.a
STATIC_PADSLIB_NM = libpadsc-g.a
# SHARED_ASTLIB_NM = libast.so.5.4
SHARED_ASTLIB_NM = libast.so
SHARED_PADSLIB_NM = libpadsc.so.1.0
SHARED_PADSLIB_NM_ALT1 = libpadsc.so.1
SHARED_PADSLIB_NM_ALT2 = libpadsc.so
STATIC_LIBTOOL = ar r
STATIC_LIBTOOL_OPTS =
SHARED_LIBTOOL = /usr/bin/cc -shared -nostartfiles
SHARED_LIBTOOL_PRE_PADSLIB = -Wl,-whole-archive
SHARED_LIBTOOL_PRE_ASTLIB  = -Wl,-no-whole-archive
SHARED_LIBTOOL_OPTS = -lc
LINKER = $(CC)
LINKOPTS = $(CDBGFLAGS) -Wl,-z,origin '-Wl,-R,$$ORIGIN/../lib'
endif

ifeq ($(OPSYS),irix)
CC = /home/gsf/arch/$(AST_ARCH)/bin/cc
CDBGFLAGS = -g -woff 47,1174
ifdef BuildPADSLib
CSHAREFLAGS = -KPIC
else
CSHAREFLAGS =
endif
CARCHFLAGS =
LIB_DEP_PATTERN = %.a
STATIC_ASTLIB_NM = libast-g.a
STATIC_PADSLIB_NM = libpadsc-g.a
# SHARED_ASTLIB_NM = libast.so.5.4
SHARED_ASTLIB_NM = libast.so
SHARED_PADSLIB_NM = libpadsc.so.1.0
SHARED_PADSLIB_NM_ALT1 = libpadsc.so.1
SHARED_PADSLIB_NM_ALT2 = libpadsc.so
STATIC_LIBTOOL = ar r
STATIC_LIBTOOL_OPTS =
SHARED_LIBTOOL = /usr/bin/cc -shared -update_registry $(INSTALLROOT)/lib/registry.ld
SHARED_LIBTOOL_PRE_PADSLIB = -all
SHARED_LIBTOOL_PRE_ASTLIB  = -notall
SHARED_LIBTOOL_OPTS =
LINKER = $(CC)
LINKOPTS = $(CDBGFLAGS)
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
STATIC_PADSLIB = $(INSTALLROOT)/lib/$(STATIC_PADSLIB_NM)
STATIC_ASTLIB = $(ASTLIB_DIR)/$(STATIC_ASTLIB_NM)
STATIC_LIBS = $(STATIC_PADSLIB) $(STATIC_ASTLIB) 
LIB_DEPS = $(STATIC_LIBS)

DYNAMIC_LIBS = -L $(ASTLIB_DIR) -L $(INSTALLROOT)/lib -lpadsc-g -last-g
SHARED_PADSLIB_DEP = $(INSTALLROOT)/lib/$(SHARED_PADSLIB_NM)
SHARED_ASTLIB_DEP = $(ASTLIB_DIR)/$(SHARED_ASTLIB_NM)
# DYNAMIC_LIB_DEPS = $(SHARED_PADSLIB_DEP) $(SHARED_ASTLIB_DEP)

INCLUDES = -I/home/gsf/arch/$(AST_ARCH)/include/ast -I$(PADS_HOME)/include -I. -I..
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

CFLAGS = $(CDBGFLAGS) $(CARCHFLAGS) $(INCLUDES)
COMPILE = $(CC) $(CSHAREFLAGS) $(CFLAGS) $(DEFS) $(SHAREDEFS)
MKSRC = $(CC) -E $(CFLAGS)
LINK = $(LINKER) $(LINKOPTS)

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
  for file in $(LIB_DEPS); do \
    if [ ! -e $$file ]; then \
      echo "UNEXPECTED: library $$file does not exist"; \
      exit 1; \
    fi; \
  done; \
)
endef

define LibSanityCheck
( for file in $(LIB_DEPS); do \
    if [ ! -e $$file ]; then \
      echo "UNEXPECTED: library $$file does not exist"; \
      exit 1; \
    fi; \
  done; \
)
endef

define CCExec_DYNAMIC
(set -x; \
 $(RM) $@; \
 $(COMPILE) $(patsubst $(LIB_DEP_PATTERN),,$(patsubst %.h,,$^)) $(DYNAMIC_LIBS) -o $@; \
)
endef

define CCExec_STATIC
(set -x; \
 $(RM) $@; \
 $(COMPILE) $(patsubst $(LIB_DEP_PATTERN),,$(patsubst %.h,,$^)) $(STATIC_LIBS) -o $@; \
)
endef

define RegressPre
(echo " "; echo "Performing $@"; \
  if [ -e tmp ]; then echo -n "";else mkdir tmp; fi; \
  $(RM) tmp/tmp.$<; \
)
endef

define RegressPost
(echo diff tmp/tmp.$< ../../regress/$<.regress; diff tmp/tmp.$< ../../regress/$<.regress || echo $< DIFFERS; \
)
endef

define RegressDef
(echo " "; echo "Performing $@"; \
  if [ -e tmp ]; then echo -n "";else mkdir tmp; fi; \
  $(RM) tmp/tmp.$<$$suf; \
  echo "(./$< $$args 2>&1) | $(PADS_HOME)/scripts/remove_junk.pl | cat > tmp/tmp.$<$$suf"; \
  (./$< $$args 2>&1) | $(PADS_HOME)/scripts/remove_junk.pl | cat > tmp/tmp.$<$$suf; \
  echo diff tmp/tmp.$<$$suf ../../regress/$<.regress$$suf; diff tmp/tmp.$<$$suf ../../regress/$<.regress$$suf || echo $<$$suf DIFFERS; \
)
endef

define RegressInput
(echo " "; echo "Performing $@"; \
  if [ -e tmp ]; then echo -n "";else mkdir tmp; fi; \
  $(RM) tmp/tmp.$<$$suf; \
  echo "(./$< $$args < $$input 2>&1) | $(PADS_HOME)/scripts/remove_junk.pl | cat > tmp/tmp.$<$$suf"; \
  (./$< $$args < $$input 2>&1) | $(PADS_HOME)/scripts/remove_junk.pl | cat > tmp/tmp.$<$$suf; \
  echo diff tmp/tmp.$<$$suf ../../regress/$<.regress$$suf; diff tmp/tmp.$<$$suf ../../regress/$<.regress$$suf || echo $<$$suf DIFFERS; \
)
endef

define RegressFilter
(echo " "; echo "Performing $@"; \
  if [ -e tmp ]; then echo -n "";else mkdir tmp; fi; \
  $(RM) tmp/tmp.$<$$suf; \
  echo "(./$< $$args 2>&1) | $(PADS_HOME)/scripts/remove_junk.pl | grep $$filter | cat > tmp/tmp.$<$$suf"; \
  (./$< $$args 2>&1) | $(PADS_HOME)/scripts/remove_junk.pl | grep $$filter | cat > tmp/tmp.$<$$suf; \
  echo diff tmp/tmp.$<$$suf ../../regress/$<.regress$$suf; diff tmp/tmp.$<$$suf ../../regress/$<.regress$$suf || echo $<$$suf DIFFERS; \
)
endef

.SUFFIXES:
.SUFFIXES: .c .o

ifdef BuildPADSLib
%.o: %.c $(INCLUDE_DEPS_ADD) $(INCLUDE_DEPS)
	@echo "Using rules.mk rule A"
	@echo "INCLUDE_DEPS_ADD = $(INCLUDE_DEPS_ADD)"
	@echo "INCLUDE_DEPS = $(INCLUDE_DEPS)"
	$(COMPILE) -c $<
else
%: %.o $(LIB_DEPS)
	@echo "Using rules.mk rule J"
	$(LINK) $< $(DYNAMIC_LIBS) -o $@

%.o: %.c $(INCLUDE_DEPS)
	@echo "Using rules.mk rule K"
	$(COMPILE) -c $<

%: %.c $(INCLUDE_DEPS) $(LIB_DEPS)
	@echo "Using rules.mk rule L"
	$(COMPILE) $< $(DYNAMIC_LIBS) -o $@
endif

ifdef GEN_DIR
$(GEN_DIR)/%.c: %.p $(PADSC) $(PADSC_REAL)
	@echo "Using rule P"
	$(PADSC) $< -r $(GEN_DIR) -I . -I ..
endif

# Notes: 
#   Example linux build using nmake
#     + cc -g -Wall -I../../../include -I- -I/home/gsf/arch/linux.i386/include/ast -D_PACKAGE_ast -c ../ebcdic_test.c
#     + cc -g -Wall -Wl,-z,origin '-Wl,-R,$ORIGIN/../lib' -o ebcdic_test ebcdic_test.o /home/gruber/knb/arch/linux.i386/lib/libpadsc-g.a /home/gsf/arch/linux.i386/lib/libast-g.a
#
#   Example sgi build using nmake
#
#

