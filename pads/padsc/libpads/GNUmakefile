
 # This is a GNU makefile.

ifndef PADS_HOME
%: forceabort2
	@echo "ERROR: env variable PADS_HOME must be defined"
	@exit 1
forceabort2: ;
endif

ifndef AST_ARCH
  AST_ARCH := $(shell $(PADS_HOME)/ast-ast/bin/package.cvs)
  export AST_ARCH
endif

CURDIR := $(shell pwd)
ifneq ($(AST_ARCH),$(notdir $(CURDIR)))
include $(PADS_HOME)/mk/redirect.mk
else

 # The following rules are run from the arch directory

SCRIPTS = $(PADS_HOME)/scripts
BuildPADSLib = 1
INCLUDE_DEPS_ADD = *-gen.h
include $(PADS_HOME)/mk/rules.mk

VPATH = ..

INCDIR = $(PADS_HOME)/padsc/include
PCGEN_IN = $(INCDIR)/pcgen-macros.h
PCGEN = $(INCDIR)/pcgen-macros-gen.h

OBJS_O = rbuf.o default_io_disc.o pads-gen.o pads-acc-gen.o pads-read-gen.o pads-write-gen.o pads-misc-gen.o

OBJS_D = $(OBJS_O:%.o=%_d.o)

GEN_FILES = pads-acc-gen.c \
            pads-misc-gen.c \
            pads-acc-macros-gen.h  \
            pads-misc-macros-gen.h \
            pads-gen.c             \
            pads-read-gen.c \
            pads-write-gen.c \
            pads-macros-gen.h      \
            pads-read-macros-gen.h \
            pads-write-macros-gen.h

all :
	@$(MAKE) -f ../GNUmakefile dogen
	@$(MAKE) -f ../GNUmakefile real_all

opt :
	@$(MAKE) -f ../GNUmakefile dogen
	@$(MAKE) -f ../GNUmakefile real_opt

debug :
	@$(MAKE) -f ../GNUmakefile dogen
	@$(MAKE) -f ../GNUmakefile real_debug

real_all : $(STATIC_PADSLIB_NM_O) $(SHARED_PADSLIB_NM_O) $(STATIC_PADSLIB_NM_D) $(SHARED_PADSLIB_NM_D)

real_opt : $(STATIC_PADSLIB_NM_O) $(SHARED_PADSLIB_NM_O)

real_debug : $(STATIC_PADSLIB_NM_D) $(SHARED_PADSLIB_NM_D)

$(STATIC_PADSLIB_NM_O) : $(OBJS_O)
	$(STATIC_LIBTOOL) $(STATIC_PADSLIB_NM_O) $(STATIC_LIBTOOL_OPTS) $(OBJS_O)

$(SHARED_PADSLIB_NM_O) : $(STATIC_PADSLIB_NM_O)
	$(SHARED_LIBTOOL) -o $(SHARED_PADSLIB_NM_O) $(SHARED_LIBTOOL_WHOLE_ARCHIVE) \
        $(STATIC_PADSLIB_NM_O) $(SHARED_LIBTOOL_NOT_WHOLE_ARCHIVE) $(SHARED_LIBTOOL_OPTS)

$(STATIC_PADSLIB_NM_D) : $(OBJS_D)
	$(STATIC_LIBTOOL) $(STATIC_PADSLIB_NM_D) $(STATIC_LIBTOOL_OPTS) $(OBJS_D)

$(SHARED_PADSLIB_NM_D) : $(STATIC_PADSLIB_NM_D)
	$(SHARED_LIBTOOL) -o $(SHARED_PADSLIB_NM_D) $(SHARED_LIBTOOL_WHOLE_ARCHIVE) \
        $(STATIC_PADSLIB_NM_D) $(SHARED_LIBTOOL_NOT_WHOLE_ARCHIVE) $(SHARED_LIBTOOL_OPTS)

install : all
	+@(set -x ; \
	$(PADS_HOME)/scripts/install-sh -c -m 444 $(STATIC_PADSLIB_NM_O) $(INSTALLROOT)/lib; \
	$(PADS_HOME)/scripts/install-sh -c -m 555 $(SHARED_PADSLIB_NM_O) $(INSTALLROOT)/lib; \
	$(PADS_HOME)/scripts/install-sh -c -m 444 $(STATIC_PADSLIB_NM_D) $(INSTALLROOT)/lib; \
	$(PADS_HOME)/scripts/install-sh -c -m 555 $(SHARED_PADSLIB_NM_D) $(INSTALLROOT)/lib; \
	( cd $(INSTALLROOT)/lib; \
	  $(RM) $(SHARED_PADSLIB_NM_ALT1_O); \
	  ln $(SHARED_PADSLIB_NM_O) $(SHARED_PADSLIB_NM_ALT1_O); \
	  $(RM) $(SHARED_PADSLIB_NM_ALT2_O); \
	  ln $(SHARED_PADSLIB_NM_O) $(SHARED_PADSLIB_NM_ALT2_O); \
	  $(RM) $(SHARED_PADSLIB_NM_ALT1_D); \
	  ln $(SHARED_PADSLIB_NM_D) $(SHARED_PADSLIB_NM_ALT1_D); \
	  $(RM) $(SHARED_PADSLIB_NM_ALT2_D); \
	  ln $(SHARED_PADSLIB_NM_D) $(SHARED_PADSLIB_NM_ALT2_D); \
	  ) )
	@(cd $(INCDIR); $(MAKE) install)

dogen : $(PCGEN) $(SCRIPTS)/mksrc.pl $(PADS_HOME)/padsc/libpads/pads.c $(INCLUDE_DEPS)
	@echo "Building generated source files"
	$(SCRIPTS)/mksrc.pl $(PADS_HOME)/padsc/libpads/pads.c $(CURDIR) $(PADS_HOME)
	touch dogen

$(PCGEN) : $(SCRIPTS)/mksrc.pl $(PCGEN_IN)
	@(cd $(INCDIR); $(MAKE) all)

pads-gen-g.o: pads-gen.c $(INCLUDE_DEPS)
ifdef DEBUG_RULES_MK
	@echo "Using rules.mk rule A-mod-D"
endif
	$(COMPILE_D) -c $< -o $@

pads-gen.o: pads-gen.c $(INCLUDE_DEPS)
ifdef DEBUG_RULES_MK
	@echo "Using rules.mk rule A-mod-O"
endif
	$(COMPILE_O) -c $< -o $@

clean :
	$(RM) $(STATIC_PADSLIB_NM_O) $(SHARED_PADSLIB_NM_O) $(STATIC_PADSLIB_NM_D) $(SHARED_PADSLIB_NM_D) *~ *.o ../*~

veryclean : clean
	$(RM) dogen $(GEN_FILES)

 # End of rules that are run from the arch directory
endif
