
 # This is a GNU makefile.  See Makefile for the nmake version.

PADS_HOME = ..
ifndef AST_ARCH
  AST_ARCH := $(shell $(PADS_HOME)/scripts/package)
  export AST_ARCH
endif

CURDIR := $(shell pwd)
ifneq ($(AST_ARCH),$(notdir $(CURDIR)))
include $(PADS_HOME)/mk/redirect.mk
else

 # The following rules are run from the arch directory

PADS_HOME = ../..
SCRIPTS = $(PADS_HOME)/scripts
BuildPADSLib = 1
INCLUDE_DEPS_ADD = libpadsc-macros-gen.h
include $(PADS_HOME)/mk/rules.mk

VPATH = ..

OBJS = rbuf.o default_io_disc.o libpadsc-gen.o libpadsc-acc-gen.o libpadsc-read-gen.o libpadsc-write-gen.o libpadsc-cobol-read-gen.o libpadsc-cobol-write-gen.o libpadsc-misc-gen.o

GEN_FILES = libpadsc-acc-gen.c \
            libpadsc-misc-gen.c \
            libpadsc-acc-macros-gen.h  \
            libpadsc-misc-macros-gen.h \
            libpadsc-gen.c             \
            libpadsc-read-gen.c \
            libpadsc-write-gen.c \
            libpadsc-cobol-read-gen.c \
            libpadsc-cobol-write-gen.c \
            libpadsc-macros-gen.h      \
            libpadsc-read-macros-gen.h \
            libpadsc-write-macros-gen.h

all :
	@$(MAKE) -f ../GNUmakefile dogen
	@$(MAKE) -f ../GNUmakefile real_all

real_all : $(STATIC_PADSLIB_NM) $(SHARED_PADSLIB_NM)

$(STATIC_PADSLIB_NM) : $(OBJS)
	$(STATIC_LIBTOOL) $(STATIC_PADSLIB_NM) $(STATIC_LIBTOOL_OPTS) $(OBJS)

$(SHARED_PADSLIB_NM) : $(STATIC_PADSLIB_NM) $(STATIC_ASTLIB)
	$(SHARED_LIBTOOL) -o $(SHARED_PADSLIB_NM) $(SHARED_LIBTOOL_PRE_PADSLIB) \
        $(STATIC_PADSLIB_NM) $(SHARED_LIBTOOL_PRE_ASTLIB) $(STATIC_ASTLIB) $(SHARED_LIBTOOL_OPTS)

install : all
	+@(set -x ; $(PADS_HOME)/scripts/install-sh -c -m 444 $(STATIC_PADSLIB_NM) $(INSTALLROOT)/lib/; \
	$(PADS_HOME)/scripts/install-sh -c -m 555 $(SHARED_PADSLIB_NM) $(INSTALLROOT)/lib/; \
	( cd $(INSTALLROOT)/lib; \
	  if (test -e $(SHARED_PADSLIB_NM_ALT1) && ! (cmp $(SHARED_PADSLIB_NM) $(SHARED_PADSLIB_NM_ALT1) > /dev/null)); then \
		$(RM) $(SHARED_PADSLIB_NM_ALT1); \
		ln $(SHARED_PADSLIB_NM) $(SHARED_PADSLIB_NM_ALT1); \
	  fi; \
	  if (test -e $(SHARED_PADSLIB_NM_ALT2) && ! (cmp $(SHARED_PADSLIB_NM) $(SHARED_PADSLIB_NM_ALT2) > /dev/null)); then \
		$(RM) $(SHARED_PADSLIB_NM_ALT2); \
		ln $(SHARED_PADSLIB_NM) $(SHARED_PADSLIB_NM_ALT2); \
	  fi ); \
	for file in libpadsc.h rbuf.h libpadsc-private.h pdc_io_disc.h; do \
		$(PADS_HOME)/scripts/install-sh -c -m 444 $(PADS_HOME)/include/$$file $(INSTALLROOT)/include/; \
	done;)

dogen : $(SCRIPTS)/mksrc.pl $(PADS_HOME)/libpadsc/libpadsc.c $(INCLUDE_DEPS)
	@echo "Building generated source files"
	$(SCRIPTS)/mksrc.pl $(PADS_HOME)/libpadsc/libpadsc.c $(CURDIR) $(PADS_HOME)
	touch dogen

libpadsc-gen.o: libpadsc-gen.c $(INCLUDE_DEPS)
	@echo "Using rules.mk rule A-mod"
	$(COMPILE) -c $<

clean :
	$(RM) libpadsc.* libpadsc-g.* *~ *.o ../*~

veryclean : clean
	$(RM) dogen $(GEN_FILES)

 # End of rules that are run from the arch directory
endif
