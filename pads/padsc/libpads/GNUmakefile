
 # This is a GNU makefile.

PADS_HOME = ../..
ifndef AST_ARCH
  AST_ARCH := $(shell $(PADS_HOME)/ast-ast/bin/package.cvs)
  export AST_ARCH
endif

CURDIR := $(shell pwd)
ifneq ($(AST_ARCH),$(notdir $(CURDIR)))
include $(PADS_HOME)/mk/redirect.mk
else

 # The following rules are run from the arch directory

PADS_HOME = ../../..
SCRIPTS = $(PADS_HOME)/scripts
BuildPADSLib = 1
INCLUDE_DEPS_ADD = *-gen.h
include $(PADS_HOME)/mk/rules.mk

VPATH = ..

OBJS_O = rbuf.o default_io_disc.o padsc-gen.o padsc-acc-gen.o padsc-read-gen.o padsc-write-gen.o padsc-misc-gen.o

OBJS_D = $(OBJS_O:%.o=%-g.o)

GEN_FILES = padsc-acc-gen.c \
            padsc-misc-gen.c \
            padsc-acc-macros-gen.h  \
            padsc-misc-macros-gen.h \
            padsc-gen.c             \
            padsc-read-gen.c \
            padsc-write-gen.c \
            padsc-macros-gen.h      \
            padsc-read-macros-gen.h \
            padsc-write-macros-gen.h

INSTALL_INCLUDES = padsc.h rbuf.h padsc-private.h pdc_io_disc.h

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

$(SHARED_PADSLIB_NM_O) : $(STATIC_PADSLIB_NM_O) $(STATIC_ASTLIB_O)
	$(SHARED_LIBTOOL) -o $(SHARED_PADSLIB_NM_O) $(SHARED_LIBTOOL_WHOLE_ARCHIVE) \
        $(STATIC_PADSLIB_NM_O) $(SHARED_LIBTOOL_NOT_WHOLE_ARCHIVE) $(STATIC_ASTLIB_O) $(SHARED_LIBTOOL_OPTS)

$(STATIC_PADSLIB_NM_D) : $(OBJS_D)
	$(STATIC_LIBTOOL) $(STATIC_PADSLIB_NM_D) $(STATIC_LIBTOOL_OPTS) $(OBJS_D)

$(SHARED_PADSLIB_NM_D) : $(STATIC_PADSLIB_NM_D) $(STATIC_ASTLIB_D)
	$(SHARED_LIBTOOL) -o $(SHARED_PADSLIB_NM_D) $(SHARED_LIBTOOL_WHOLE_ARCHIVE) \
        $(STATIC_PADSLIB_NM_D) $(SHARED_LIBTOOL_NOT_WHOLE_ARCHIVE) $(STATIC_ASTLIB_D) $(SHARED_LIBTOOL_OPTS)

install : all
	+@(set -x ; \
	$(PADS_HOME)/scripts/install-sh -c -m 444 $(STATIC_PADSLIB_NM_O) $(INSTALLROOT)/lib/; \
	$(PADS_HOME)/scripts/install-sh -c -m 555 $(SHARED_PADSLIB_NM_O) $(INSTALLROOT)/lib/; \
	$(PADS_HOME)/scripts/install-sh -c -m 444 $(STATIC_PADSLIB_NM_D) $(INSTALLROOT)/lib/; \
	$(PADS_HOME)/scripts/install-sh -c -m 555 $(SHARED_PADSLIB_NM_D) $(INSTALLROOT)/lib/; \
	( cd $(INSTALLROOT)/lib; \
	  $(RM) $(SHARED_PADSLIB_NM_ALT1_O); \
	  ln $(SHARED_PADSLIB_NM_O) $(SHARED_PADSLIB_NM_ALT1_O); \
	  $(RM) $(SHARED_PADSLIB_NM_ALT2_O); \
	  ln $(SHARED_PADSLIB_NM_O) $(SHARED_PADSLIB_NM_ALT2_O); \
	  $(RM) $(SHARED_PADSLIB_NM_ALT1_D); \
	  ln $(SHARED_PADSLIB_NM_D) $(SHARED_PADSLIB_NM_ALT1_D); \
	  $(RM) $(SHARED_PADSLIB_NM_ALT2_D); \
	  ln $(SHARED_PADSLIB_NM_D) $(SHARED_PADSLIB_NM_ALT2_D); \
	  ); \
	for file in $(INSTALL_INCLUDES); do \
		$(PADS_HOME)/scripts/install-sh -c -m 444 $(PADS_HOME)/padsc/include/$$file $(INSTALLROOT)/include/; \
	done;)

dogen : $(SCRIPTS)/mksrc.pl $(PADS_HOME)/padsc/libpadsc/padsc.c $(INCLUDE_DEPS)
	@echo "Building generated source files"
	$(SCRIPTS)/mksrc.pl $(PADS_HOME)/padsc/libpadsc/padsc.c $(CURDIR) $(PADS_HOME)
	touch dogen

padsc-gen-g.o: padsc-gen.c $(INCLUDE_DEPS)
ifdef DEBUG_RULES_MK
	@echo "Using rules.mk rule A-mod-D"
endif
	$(COMPILE_D) -c $< -o $@

padsc-gen.o: padsc-gen.c $(INCLUDE_DEPS)
ifdef DEBUG_RULES_MK
	@echo "Using rules.mk rule A-mod-O"
endif
	$(COMPILE_O) -c $< -o $@

clean :
	$(RM) $(STATIC_PADSLIB_NM_O) $(SHARED_PADSLIB_NM_O) $(STATIC_PADSLIB_NM_D) $(SHARED_PADSLIB_NM_D) *~ *.o ../*~

veryclean : clean
	$(RM) dogen $(GEN_FILES)
	(cd $(INSTALLROOT)/include; $(RM) $(INSTALL_INCLUDES); )

 # End of rules that are run from the arch directory
endif
