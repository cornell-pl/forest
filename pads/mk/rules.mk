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
# If USE_GALAX is defined, the padsc option -x is included
# and the pglx library is added to the set of libraries to link against.
# Also, the GALAX include paths are added as -I
# options and the appropriate ocaml and Galax libraries are added
# to the set of libraries to link against. In addition, the compilation
# flag -DUSE_GALAX is added.

# uncomment this to debug rules.mk
# DEBUG_RULES_MK = 1

# uncomment this once we build a shared ast library
# HAVE_SHARED_ASTLIB = 1

ifndef INSTALLROOT
%: forceabort2
	@echo "ERROR: env variable INSTALLROOT must be defined"
	@exit 1
forceabort2: ;
endif

LIBDIR = $(INSTALLROOT)/lib

ifndef AST_ARCH
AST_ARCH := $(shell $(PADS_HOME)/ast-ast/bin/package.cvs)
export AST_ARCH
endif

FORCE_RESULT := $(shell $(PADS_HOME)/scripts/getprobeinfo.tcsh)
include $(PADS_HOME)/mk/rules.arch.$(AST_ARCH).mk

ifndef AST_HOME
AST_HOME := $(PADS_HOME)/ast-ast/arch/$(AST_ARCH)
export AST_HOME
endif

ifndef GALAX_HOME
GALAX_HOME := /home/mff/Galax
export GALAX_HOME
endif
GALAX_LIB_DIR = $(GALAX_HOME)/lib/c

ifndef PADSGLX_HOME
PADSGLX_HOME := /home/mff/pads-glx/api
export PADSGLX_HOME
endif
PADSGLX_LIB_DIR = $(PADSGLX_HOME)

ifndef OCAML_LIB_DIR
OCAML_LIB_DIR = /usr/lib/ocaml
export OCAML_LIB_DIR
endif

ifndef USR_LIB_DIR
USR_LIB_DIR = /usr/lib
endif

ARCH_N_OPSYS = $(shell $(PADS_HOME)/scripts/arch-n-opsys)
OPSYS = $(shell $(PADS_HOME)/scripts/opsys)

LIB_DEP_PATTERN = %$(mam_cc_SUFFIX_ARCHIVE)

STATIC_ASTLIB_NM_O = $(mam_cc_PREFIX_ARCHIVE)ast$(mam_cc_SUFFIX_ARCHIVE)
STATIC_ASTLIB_NM_D = $(mam_cc_PREFIX_ARCHIVE)ast$(mam_cc_SUFFIX_ARCHIVE)

STATIC_PADSLIB_NM_O = $(mam_cc_PREFIX_ARCHIVE)pads$(mam_cc_SUFFIX_ARCHIVE)
STATIC_PADSLIB_NM_D = $(mam_cc_PREFIX_ARCHIVE)pads-g$(mam_cc_SUFFIX_ARCHIVE)

STATIC_PGLXLIB_NM_O = $(mam_cc_PREFIX_ARCHIVE)pglx$(mam_cc_SUFFIX_ARCHIVE)
STATIC_PGLXLIB_NM_D = $(mam_cc_PREFIX_ARCHIVE)pglx-g$(mam_cc_SUFFIX_ARCHIVE)

ifdef HAVE_SHARED_ASTLIB
SHARED_ASTLIB_NM_O = $(mam_cc_PREFIX_SHARED)ast$(mam_cc_SUFFIX_SHARED)
SHARED_ASTLIB_NM_D = $(mam_cc_PREFIX_SHARED)ast$(mam_cc_SUFFIX_SHARED)
else
SHARED_ASTLIB_NM_O = $(STATIC_ASTLIB_NM_O)
SHARED_ASTLIB_NM_D = $(STATIC_ASTLIB_NM_D)
endif

SHARED_PADSLIB_NM_O = $(mam_cc_PREFIX_SHARED)pads$(mam_cc_SUFFIX_SHARED).1.0
SHARED_PADSLIB_NM_ALT1_O = $(mam_cc_PREFIX_SHARED)pads$(mam_cc_SUFFIX_SHARED).1
SHARED_PADSLIB_NM_ALT2_O = $(mam_cc_PREFIX_SHARED)pads$(mam_cc_SUFFIX_SHARED)

SHARED_PADSLIB_NM_D = $(mam_cc_PREFIX_SHARED)pads-g$(mam_cc_SUFFIX_SHARED).1.0
SHARED_PADSLIB_NM_ALT1_D = $(mam_cc_PREFIX_SHARED)pads-g$(mam_cc_SUFFIX_SHARED).1
SHARED_PADSLIB_NM_ALT2_D = $(mam_cc_PREFIX_SHARED)pads-g$(mam_cc_SUFFIX_SHARED)

SHARED_PGLXLIB_NM_O = $(mam_cc_PREFIX_SHARED)pglxc$(mam_cc_SUFFIX_SHARED).1.0
SHARED_PGLXLIB_NM_ALT1_O = $(mam_cc_PREFIX_SHARED)pglxc$(mam_cc_SUFFIX_SHARED).1
SHARED_PGLXLIB_NM_ALT2_O = $(mam_cc_PREFIX_SHARED)pglxc$(mam_cc_SUFFIX_SHARED)

SHARED_PGLXLIB_NM_D = $(mam_cc_PREFIX_SHARED)pglxc-g$(mam_cc_SUFFIX_SHARED).1.0
SHARED_PGLXLIB_NM_ALT1_D = $(mam_cc_PREFIX_SHARED)pglxc-g$(mam_cc_SUFFIX_SHARED).1
SHARED_PGLXLIB_NM_ALT2_D = $(mam_cc_PREFIX_SHARED)pglxc-g$(mam_cc_SUFFIX_SHARED)


CC = $(mam_cc_CC)
CDBGFLAGS = $(mam_cc_WARN) $(mam_cc_DEBUG)
COPTFLAGS = $(mam_cc_WARN) $(mam_cc_OPTIMIZE) -DNDEBUG
ifdef BuildPADSLib
CSHAREFLAGS = $(mam_cc_DLL)
else
CSHAREFLAGS =
endif
# XXX Nothing for these in rules.arch.<ARCH>.mk ???
CARCHFLAGS =
STATIC_LIBTOOL = ar r
STATIC_LIBTOOL_OPTS =

SHARED_LIBTOOL = $(CC) $(mam_cc_SHARED) $(mam_cc_SHARED_REGISTRY)
SHARED_LIBTOOL_WHOLE_ARCHIVE = $(mam_cc_LIB_ALL)
SHARED_LIBTOOL_NOT_WHOLE_ARCHIVE = $(mam_cc_LIB_UNDEF)
SHARED_LIBTOOL_OPTS = $(mam_cc_DLL_LIBRARIES)

LINKER = $(mam_cc_LD)
LINKOPTS_D = $(CDBGFLAGS) $(mam_cc_LD_ORIGIN)
LINKOPTS_O = $(COPTFLAGS) $(mam_cc_LD_ORIGIN)

empty:=
space:=$(empty) $(empty)

# OS specific rules
# (may override some of the above)

ifeq ($(OPSYS),irix)
COPTFLAGS := $(subst -O$(space),-O2$(space),$(COPTFLAGS))
CDBGFLAGS += -woff 47,1174,3201,3434
COPTFLAGS += -woff 47,1174,3201,3434
endif

ifeq ($(OPSYS),linux)
COPTFLAGS := $(subst -O$(space),-O2$(space),$(COPTFLAGS))
endif

# Done with architecture-specific stuff

ifdef USE_GALAX
PADSC_EXTRA = -x
CDBGFLAGS += -DUSE_GALAX
COPTFLAGS += -DUSE_GALAX
endif

STATIC_PADSLIB_O = $(LIBDIR)/$(STATIC_PADSLIB_NM_O)
STATIC_PGLXLIB_O = $(LIBDIR)/$(STATIC_PGLXLIB_NM_O)
STATIC_ASTLIB_O = $(LIBDIR)/$(STATIC_ASTLIB_NM_O)
# mff may need to change next two defns
STATIC_GALAXLIB_O = $(PADSGLX_LIB_DIR)/libpadsglxopt$(mam_cc_SUFFIX_ARCHIVE)
STATIC_OCAMLLIB_O = \
  $(OCAML_LIB_DIR)/libnums$(mam_cc_SUFFIX_ARCHIVE) \
  $(USR_LIB_DIR)/libm$(mam_cc_SUFFIX_ARCHIVE) \
  $(USR_LIB_DIR)/libdl$(mam_cc_SUFFIX_ARCHIVE) \
  $(USR_LIB_DIR)/libcurses$(mam_cc_SUFFIX_ARCHIVE) \
  $(OCAML_LIB_DIR)/libunix$(mam_cc_SUFFIX_ARCHIVE) \
  $(OCAML_LIB_DIR)/libstr$(mam_cc_SUFFIX_ARCHIVE)
# XXX what about $(mam_cc_PREFIX_ARCHIVE)camlrun$(mam_cc_SUFFIX_ARCHIVE) ?
ifdef USE_GALAX
STATIC_LIBS_O = $(STATIC_PGLXLIB_O) $(STATIC_PADSLIB_O) $(STATIC_ASTLIB_O) 
else
STATIC_LIBS_O = $(STATIC_PADSLIB_O) $(STATIC_ASTLIB_O) 
endif
ifdef USE_GALAX
STATIC_LIBS_O += $(STATIC_GALAXLIB_O) $(STATIC_OCAMLLIB_O)
endif
LIB_DEPS_O = $(STATIC_LIBS_O)

STATIC_PADSLIB_D = $(LIBDIR)/$(STATIC_PADSLIB_NM_D)
STATIC_PGLXLIB_D = $(LIBDIR)/$(STATIC_PGLXLIB_NM_D)
STATIC_ASTLIB_D = $(LIBDIR)/$(STATIC_ASTLIB_NM_D)
# mff may need to change next two defns
STATIC_GALAXLIB_D = $(PADSGLX_LIB_DIR)/libpadsglxopt$(mam_cc_SUFFIX_ARCHIVE)
STATIC_OCAMLLIB_D = $(STATIC_OCAMLLIB_O) # no debug versions available
ifdef USE_GALAX
STATIC_LIBS_D = $(STATIC_PGLXLIB_D)
endif
STATIC_LIBS_D += $(STATIC_PADSLIB_D) $(STATIC_ASTLIB_D) 
ifdef USE_GALAX
STATIC_LIBS_D += $(STATIC_GALAXLIB_D) $(STATIC_OCAMLLIB_D)
endif
LIB_DEPS_D = $(STATIC_LIBS_D)

ifdef HAVE_SHARED_ASTLIB
SHARED_ASTLIB_O = -last
SHARED_ASTLIB_D = -last
else
SHARED_ASTLIB_O = $(STATIC_ASTLIB_O)
SHARED_ASTLIB_D = $(STATIC_ASTLIB_D)
endif

DYNAMIC_LIBS_O = -L $(LIBDIR)
ifdef USE_GALAX
DYNAMIC_LIBS_O += -lpglx
endif
DYNAMIC_LIBS_O += -lpads $(SHARED_ASTLIB_O)
ifdef USE_GALAX
# mff may need to change next line
DYNAMIC_LIBS_O += -L $(PADSGLX_LIB_DIR) -lpadsglxopt -L $(OCAML_LIB_DIR) -lnums -lm -ldl -lcurses -lunix -lstr
endif
SHARED_PADSLIB_DEP_O = $(LIBDIR)/$(SHARED_PADSLIB_NM_O)
SHARED_PGLXLIB_DEP_O = $(LIBDIR)/$(SHARED_PGLXLIB_NM_O)
SHARED_ASTLIB_DEP_O = $(LIBDIR)/$(SHARED_ASTLIB_NM_O)
ifdef USE_GALAX
DYNAMIC_LIB_DEPS_O = $(SHARED_PGLXLIB_DEP_O) $(SHARED_PADSLIB_DEP_O) $(SHARED_ASTLIB_DEP_O)
else
DYNAMIC_LIB_DEPS_O = $(SHARED_PADSLIB_DEP_O) $(SHARED_ASTLIB_DEP_O)
endif
ifdef USE_GALAX
# only statics available
DYNAMIC_LIB_DEPS_O += $(STATIC_GALAXLIB_O) $(STATIC_OCAMLLIB_O)
endif

DYNAMIC_LIBS_D = -L $(LIBDIR)
ifdef USE_GALAX
DYNAMIC_LIBS_D += -lpglx-g
endif
DYNAMIC_LIBS_D += -lpads-g  $(SHARED_ASTLIB_D)
ifdef USE_GALAX
# mff may need to change next line 
DYNAMIC_LIBS_D += -L $(PADSGLX_LIB_DIR) -lpadsglxopt -L $(OCAML_LIB_DIR) -lnums -lm -ldl -lcurses -lunix -lstr
# XXX what about -lcamlrun ?
endif
SHARED_PADSLIB_DEP_D = $(LIBDIR)/$(SHARED_PADSLIB_NM_D)
SHARED_PGLXLIB_DEP_D = $(LIBDIR)/$(SHARED_PGLXLIB_NM_D)
SHARED_ASTLIB_DEP_D = $(LIBDIR)/$(SHARED_ASTLIB_NM_D)
ifdef USE_GALAX
DYNAMIC_LIB_DEPS_D = $(SHARED_PGLXLIB_DEP_D)
endif
DYNAMIC_LIB_DEPS_D += $(SHARED_PADSLIB_DEP_D) $(SHARED_ASTLIB_DEP_D)
ifdef USE_GALAX
# only statics available
DYNAMIC_LIB_DEPS_D += $(STATIC_GALAXLIB_D) $(STATIC_OCAMLLIB_D)
endif

TRIV_LIBS = -L $(LIBDIR) $(SHARED_ASTLIB_D)

INCLUDES =  -I. -I.. -I$(AST_HOME)/include/ast
ifdef GEN_DIR
INCLUDES += -I$(GEN_DIR)
endif

ifdef USE_GALAX
INCLUDES +=  -I$(GALAX_LIB_DIR) -I$(PADSGLX_LIB_DIR) -I$(OCAML_LIB_DIR)
endif

ifndef BuildAST4PADSLib
INCLUDES += -I$(PADS_HOME)/padsc/include
INCLUDE_DEPS = $(PADS_HOME)/padsc/include/*.h $(PADS_HOME)/padsc/include/template/*.h
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

define TrivCCExec_O
(set -x; \
 $(RM) $@; \
 $(COMPILE_O) $^ $(TRIV_LIBS) -o $@; \
)
endef

define TrivCCExec_D
(set -x; \
 $(RM) $@; \
 $(COMPILE_D) $^ $(TRIV_LIBS) -o $@; \
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
  regfile=`echo ../../regress/$<.regress$$suf | sed -e 's|_d[.]regress|.regress|'`; \
  echo "(./$< $$args 2>&1) | $(PADS_HOME)/scripts/remove_junk.pl | cat > tmp/tmp.$<$$suf"; \
  (./$< $$args 2>&1) | $(PADS_HOME)/scripts/remove_junk.pl | cat > tmp/tmp.$<$$suf; \
  echo diff tmp/tmp.$<$$suf $$regfile; diff tmp/tmp.$<$$suf $$regfile || echo "**********" $<$$suf DIFFERS; \
  echo " "; )
endef

define RegressInput
(echo " "; echo "Performing $@"; \
  if [ -e tmp ]; then echo -n "";else mkdir tmp; fi; \
  $(RM) tmp/tmp.$<$$suf; \
  regfile=`echo ../../regress/$<.regress$$suf | sed -e 's|_d[.]regress|.regress|'`; \
  echo "(./$< $$args < $$input 2>&1) | $(PADS_HOME)/scripts/remove_junk.pl | cat > tmp/tmp.$<$$suf"; \
  (./$< $$args < $$input 2>&1) | $(PADS_HOME)/scripts/remove_junk.pl | cat > tmp/tmp.$<$$suf; \
  echo diff tmp/tmp.$<$$suf $$regfile; diff tmp/tmp.$<$$suf $$regfile || echo "**********" $<$$suf DIFFERS; \
  echo " "; )
endef

define RegressInputPP
(echo " "; echo "Performing $@"; \
  if [ -e tmp ]; then echo -n "";else mkdir tmp; fi; \
  $(RM) tmp/tmp.$<$$suf; \
  regfile=`echo ../../regress/$<.regress$$suf | sed -e 's|_d[.]regress|.regress|'`; \
  echo "(cat $$input | $$pp | ./$< $$args 2>&1) | $(PADS_HOME)/scripts/remove_junk.pl | cat > tmp/tmp.$<$$suf"; \
  (cat $$input | $$pp | ./$< $$args 2>&1) | $(PADS_HOME)/scripts/remove_junk.pl | cat > tmp/tmp.$<$$suf; \
  echo diff tmp/tmp.$<$$suf $$regfile; diff tmp/tmp.$<$$suf $$regfile || echo "**********" $<$$suf DIFFERS; \
  echo " "; )
endef

define RegressFilter
(echo " "; echo "Performing $@"; \
  if [ -e tmp ]; then echo -n "";else mkdir tmp; fi; \
  $(RM) tmp/tmp.$<$$suf; \
  regfile=`echo ../../regress/$<.regress$$suf | sed -e 's|_d[.]regress|.regress|'`; \
  echo "(./$< $$args 2>&1) | $(PADS_HOME)/scripts/remove_junk.pl | grep $$filter | cat > tmp/tmp.$<$$suf"; \
  (./$< $$args 2>&1) | $(PADS_HOME)/scripts/remove_junk.pl | grep $$filter | cat > tmp/tmp.$<$$suf; \
  echo diff tmp/tmp.$<$$suf $$regfile; diff tmp/tmp.$<$$suf $$regfile || echo "**********" $<$$suf DIFFERS; \
  echo " "; )
endef

define RegressRW
(echo " "; echo "Performing $@"; \
  if [ -e tmp ]; then echo -n "";else mkdir tmp; fi; \
  $(RM) tmp/tmp.$<$$suf; \
  echo "./$< $$args < $$input > tmp/tmp.$<$$suf 2>/dev/null"; \
  ./$< $$args < $$input > tmp/tmp.$<$$suf 2>/dev/null; \
  echo cmp tmp/tmp.$<$$suf $$input; cmp tmp/tmp.$<$$suf $$input || echo "**********" $<$$suf DIFFERS; \
  echo " "; )
endef

.SUFFIXES:
.SUFFIXES: .c .o

ifdef GEN_DIR
ifdef GEN_WRITE

$(GEN_DIR)/%.c: %.p $(PADSC) $(PADSC_REAL)
ifdef DEBUG_RULES_MK
	@echo "Using rule P"
endif
	$(PADSC) $< $(PADSC_EXTRA) -r $(GEN_DIR) -I. -I..

else # !GEN_WRITE

$(GEN_DIR)/%.c: %.p $(PADSC) $(PADSC_REAL)
ifdef DEBUG_RULES_MK
	@echo "Using rule P-nowrite"
endif
	$(PADSC) $< $(PADSC_EXTRA) -r $(GEN_DIR) -wnone -I. -I..

endif # GEN_WRITE
endif # GEN_DIR

# First we put all the _d rules

ifdef REGRESS_TESTS
test_%_d: $(GEN_DIR)/%.c test_%.c $(INCLUDE_DEPS) $(LIB_DEPS_D)
ifdef DEBUG_RULES_MK
	@echo "Using rules.mk rule R_D"
endif
	@$(CCExec_DYNAMIC_D)

rw_%_d: $(GEN_DIR)/%.c rw_%.c $(INCLUDE_DEPS) $(LIB_DEPS_D)
ifdef DEBUG_RULES_MK
	@echo "Using rules.mk rule RW_D"
endif
	@$(CCExec_DYNAMIC_D)

rwxml_%_d: $(GEN_DIR)/%.c rwxml_%.c $(INCLUDE_DEPS) $(LIB_DEPS_D)
ifdef DEBUG_RULES_MK
	@echo "Using rules.mk rule RWXML_D"
endif
	@$(CCExec_DYNAMIC_D)

$(GEN_DIR)/%_expanded.c: $(GEN_DIR)/%.c
	$(MKSRC_D) $< | $(PADS_HOME)/scripts/addnl.pl > $@

test_%_dd: $(GEN_DIR)/%_expanded.c test_%.c $(INCLUDE_DEPS) $(LIB_DEPS_D)
ifdef DEBUG_RULES_MK
	@echo "Using rules.mk rule R_DD"
endif
	@$(CCExec_DYNAMIC_D)

rw_%_dd: $(GEN_DIR)/%_expanded.c rw_%.c $(INCLUDE_DEPS) $(LIB_DEPS_D)
ifdef DEBUG_RULES_MK
	@echo "Using rules.mk rule RW_DD"
endif
	@$(CCExec_DYNAMIC_D)

rwxml_%_dd: $(GEN_DIR)/%_expanded.c rwxml_%.c $(INCLUDE_DEPS) $(LIB_DEPS_D)
ifdef DEBUG_RULES_MK
	@echo "Using rules.mk rule RWXML_DD"
endif
	@$(CCExec_DYNAMIC_D)
endif # REGRESS_TESTS / _d rule

ifdef BuildPADSLib
 # Just one _d rule needed for BuildPADSLib

%_d.o: %.c $(INCLUDE_DEPS_ADD) $(INCLUDE_DEPS)
ifdef DEBUG_RULES_MK
	@echo "Using rules.mk rule A_D"
endif
	$(COMPILE_D) -c $< -o $@

else
 # Three _d rules needed for !BuildPADSLib

%_d: %_d.o $(LIB_DEPS_D)
ifdef DEBUG_RULES_MK
	@echo "Using rules.mk rule J_D"
endif
	$(LINK_D) $< $(DYNAMIC_LIBS_D) -o $@

%_d.o: %.c $(INCLUDE_DEPS_ADD) $(INCLUDE_DEPS)
ifdef DEBUG_RULES_MK
	@echo "Using rules.mk rule K_D"
endif
	$(COMPILE_D) -c $< -o $@

ifndef REGRESS_TESTS
%_d: %.c $(INCLUDE_DEPS_ADD) $(INCLUDE_DEPS) $(LIB_DEPS_D)
ifdef DEBUG_RULES_MK
	@echo "Using rules.mk rule L_D"
endif
	$(COMPILE_D) $< $(DYNAMIC_LIBS_D) -o $@
endif # !REGRESS_TESTS

endif # BuildPADSLib / _d rules

# Now the non _d rules

ifdef REGRESS_TESTS
test_%: $(GEN_DIR)/%.c test_%.c $(INCLUDE_DEPS) $(LIB_DEPS_O)
ifdef DEBUG_RULES_MK
	@echo "Using rules.mk rule R_O"
endif
	@$(CCExec_DYNAMIC_O)

rw_%: $(GEN_DIR)/%.c rw_%.c $(INCLUDE_DEPS) $(LIB_DEPS_O)
ifdef DEBUG_RULES_MK
	@echo "Using rules.mk rule RW_O"
endif
	@$(CCExec_DYNAMIC_O)

rwxml_%: $(GEN_DIR)/%.c rwxml_%.c $(INCLUDE_DEPS) $(LIB_DEPS_O)
ifdef DEBUG_RULES_MK
	@echo "Using rules.mk rule RWXML_O"
endif
	@$(CCExec_DYNAMIC_O)
endif # REGRESS_TESTS / non _d rule

ifdef BuildPADSLib
 # Just one non _d rule needed for BuildPADSLib

%.o: %.c $(INCLUDE_DEPS_ADD) $(INCLUDE_DEPS)
ifdef DEBUG_RULES_MK
	@echo "Using rules.mk rule A_O"
endif
	$(COMPILE_O) -c $< -o $@

else
 # Three non _d rules needed for !BuildPADSLib

%: %.o $(LIB_DEPS_O)
ifdef DEBUG_RULES_MK
	@echo "Using rules.mk rule J_O"
endif
	$(LINK_O) $< $(DYNAMIC_LIBS_O) -o $@

%.o: %.c $(INCLUDE_DEPS_ADD) $(INCLUDE_DEPS)
ifdef DEBUG_RULES_MK
	@echo "Using rules.mk rule K_O"
endif
	$(COMPILE_O) -c $< -o $@

%: %.c $(INCLUDE_DEPS_ADD) $(INCLUDE_DEPS) $(LIB_DEPS_O)
ifdef DEBUG_RULES_MK
	@echo "Using rules.mk rule L_O"
endif
	$(COMPILE_O) $< $(DYNAMIC_LIBS_O) -o $@

endif # BuildPadsLib / non _d rules

