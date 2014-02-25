# SWIGRules.mk
# ------------
# This file contains Variable, Macro and Rule Definitions for processing
# SWIG Interface definitions and the subsequent generated code.  Since
# the goal is to create shared objects, it resembles the creation of
# libpads.so.

# The SWIG processor command line root
SWIG = swig -Wall $(INCLUDES)

# IMPORTANT ***
# AT&T compute servers have a common PERL/JAVA as well as a native one
# It is important to ensure the instance that is built against is the
# one that is the same as the one that is eventually run.
# Additionally, the common one is typically for a 32 bit architecture
# while the native machine may be a 64 bit machine.  Watch out.
PERL = /usr/common/bin/perl
JAVA = /usr/common/jdk/bin/java

# C compiler options for compiling SWIG Java/Perl wrapper code
SWIG_JAVA_CCOPTS =
SWIG_PERL_CCOPTS = $(shell $(PERL) -MExtUtils::Embed -e ccopts)

# CFLAGS_D = $(GPROF_FLAGS) $(VM_FLAGS) $(CDBGFLAGS) $(CARCHFLAGS) $(INCLUDES)
# COMPILE_D = $(CC) $(CSHAREFLAGS) $(CFLAGS_D) $(DEFS) $(SHAREDEFS)
COMPILE_WRAPPER_D = $(CC) $(CC_DLL) $(CFLAGS_D)
# MKSRC_D = $(CC) -E $(CFLAGS_D)
# CFLAGS_O = $(COPTFLAGS) $(CARCHFLAGS) $(INCLUDES)
# COMPILE_O = $(CC) $(CSHAREFLAGS) $(CFLAGS_O) $(DEFS) $(SHAREDEFS)
COMPILE_WRAPPER_O = $(CC) $(CC_DLL) $(CFLAGS_O)
# MKSRC_O = $(CC) -E $(CFLAGS_O)
# 
# BEGIN MACROS
# ------------
# Process a SWIG interface for inclusion in a Java Program
define SWIG_JAVA
(set -x; \
  $(RM) $@; \
  $(SWIG) -java -o $@ $^; 
)
endef

# Process a SWIG interface for inclusion in a PERL Program
define SWIG_PERL
  set -x; \
  $(RM) $@; \
  $(SWIG) -perl -o $@ $<
endef

define CC_SWIGPerlWrapper_DYNAMIC_D
( set -x;\
  $(RM) $@; \
 $(COMPILE_WRAPPER_D) $(SWIG_PERL_CCOPTS) -c $< -o $@ ;\
)
endef

define CC_SWIGPerlWrapper_DYNAMIC_O
(set -x; \
 $(RM) $@; \
 $(COMPILE_WRAPPER_O) $(SWIG_PERL_CCOPTS) -c $< -o $@ ;\
)
endef

define CC_SWIGPerlWrapper_STATIC_D
(set -x; \
 $(RM) $@; \
 $(COMPILE_D) -c $(patsubst $(LIB_DEP_PATTERN),,$(patsubst %.h,,$^)) $(STATIC_LIBS_XTRA_D) -o $@; \
)
endef

define CC_SWIGJavaWrapper_STATIC_O
(set -x; \
 $(RM) $@; \
 $(COMPILE_O) $(patsubst $(LIB_DEP_PATTERN),,$(patsubst %.h,,$^)) $(STATIC_LIBS_XTRA_O) -o $@; \
)
endef

# How to link:
# One needs both -static and -whole-archive to link in stuff like 
# 	"extern foo* bar = &baz;"
#
# Not all options have counterparts in the mamprobe output so they appear here
# verbatim for my 32bit Linux config (i.e. -Wl,-Bsymbolic, etc.).
#
define LD_SWIGLIB
(set -x; \
  $(SHARED_LIBTOOL) -v -Wl,-Bsymbolic -L$(INSTALL_LIBDIR) \
  	$(SHARED_LIBTOOL_WHOLE_ARCHIVE) -Wl,$(mam_cc_STATIC) -lpads -Wl,-dy $(SHARED_LIBTOOL_NOT_WHOLE_ARCHIVE) -last \
	$^ -o $@ ; \
)
endef

# END MACROS

# BEGIN PATTERN RULES
# -------------------

%_perlwrap.c	%.pm:	%.i $(GEN_DIR)/%.c
	@echo "RULE .i -> _perlwrap.c"
	$(SWIG_PERL)

%_perlwrap.o:	%_perlwrap.c	#$(GEN_DIR)/%.h
	@echo  "RULE PERLWRAP CC"
	@$(CC_SWIGPerlWrapper_DYNAMIC_O)

%_javawrap.c %.java:	%.i
	@echo "RULE .i -> _javawrap.c"
	@$(SWIGJAVA)

%_javawrap.o:	$(GEN_DIR)/%_javawrap.c
	@echo  "RULE JAVAWRAP CC"
	@$(CC_SWIGJavaWrapper_DYNAMIC_O)

lib%.so:     %_perlwrap.o %.o # pads lib
	@echo "RULE %_perlwrap.o -> lib%.so"
	$(LD_SWIGLIB)

%.o:    $(GEN_DIR)/%.c
	@echo "RULE SWIG .c -> .o "
	$(COMPILE_O) $(mam_cc_PICBIG) -c $< -o $@

# BEGIN CONCRETE RULES
# --------------------

pads_perlwrap.c	pads.pm:	pads.i
	@echo "RULE pads.i -> pads_perlwrap.c Concrete"
	$(SWIG_PERL)

pads_perlwrap.o:	pads_perlwrap.c	
	@echo  "RULE Concrete, PADS PERLWRAP CC"
	@$(CC_SWIGPerlWrapper_DYNAMIC_O)

libpads.so:	pads_perlwrap.o # other pads lib
	@echo "RULE LD_SWIG Concrete"
	$(LD_SWIGLIB)

