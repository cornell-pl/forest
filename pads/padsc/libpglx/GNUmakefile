
 # This is a GNU makefile.

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
BuildPGLXLib = 1
include $(PADS_HOME)/mk/rules.mk

VPATH = ..

OBJS_O = pglx.o

OBJS_D = $(OBJS_O:%.o=%-g.o)

INSTALL_INCLUDES = pglx.h

all : $(STATIC_PGLXLIB_NM_O) $(SHARED_PGLXLIB_NM_O) $(STATIC_PGLXLIB_NM_D) $(SHARED_PGLXLIB_NM_D)

opt : $(STATIC_PGLXLIB_NM_O) $(SHARED_PGLXLIB_NM_O)

debug : $(STATIC_PGLXLIB_NM_D) $(SHARED_PGLXLIB_NM_D)

$(STATIC_PGLXLIB_NM_O) : $(OBJS_O)
	$(STATIC_LIBTOOL) $(STATIC_PGLXLIB_NM_O) $(STATIC_LIBTOOL_OPTS) $(OBJS_O)

$(SHARED_PGLXLIB_NM_O) : $(STATIC_PGLXLIB_NM_O) $(STATIC_ASTLIB_O)
	$(SHARED_LIBTOOL) -o $(SHARED_PGLXLIB_NM_O) $(SHARED_LIBTOOL_WHOLE_ARCHIVE) \
        $(STATIC_PGLXLIB_NM_O) $(SHARED_LIBTOOL_NOT_WHOLE_ARCHIVE) $(STATIC_ASTLIB_O) $(SHARED_LIBTOOL_OPTS)

$(STATIC_PGLXLIB_NM_D) : $(OBJS_D)
	$(STATIC_LIBTOOL) $(STATIC_PGLXLIB_NM_D) $(STATIC_LIBTOOL_OPTS) $(OBJS_D)

$(SHARED_PGLXLIB_NM_D) : $(STATIC_PGLXLIB_NM_D) $(STATIC_ASTLIB_D)
	$(SHARED_LIBTOOL) -o $(SHARED_PGLXLIB_NM_D) $(SHARED_LIBTOOL_WHOLE_ARCHIVE) \
        $(STATIC_PGLXLIB_NM_D) $(SHARED_LIBTOOL_NOT_WHOLE_ARCHIVE) $(STATIC_ASTLIB_D) $(SHARED_LIBTOOL_OPTS)

install : all
	+@(set -x ; \
	$(PADS_HOME)/scripts/install-sh -c -m 444 $(STATIC_PGLXLIB_NM_O) $(INSTALLROOT)/lib/; \
	$(PADS_HOME)/scripts/install-sh -c -m 555 $(SHARED_PGLXLIB_NM_O) $(INSTALLROOT)/lib/; \
	$(PADS_HOME)/scripts/install-sh -c -m 444 $(STATIC_PGLXLIB_NM_D) $(INSTALLROOT)/lib/; \
	$(PADS_HOME)/scripts/install-sh -c -m 555 $(SHARED_PGLXLIB_NM_D) $(INSTALLROOT)/lib/; \
	( cd $(INSTALLROOT)/lib; \
	  $(RM) $(SHARED_PGLXLIB_NM_ALT1_O); \
	  ln $(SHARED_PGLXLIB_NM_O) $(SHARED_PGLXLIB_NM_ALT1_O); \
	  $(RM) $(SHARED_PGLXLIB_NM_ALT2_O); \
	  ln $(SHARED_PGLXLIB_NM_O) $(SHARED_PGLXLIB_NM_ALT2_O); \
	  $(RM) $(SHARED_PGLXLIB_NM_ALT1_D); \
	  ln $(SHARED_PGLXLIB_NM_D) $(SHARED_PGLXLIB_NM_ALT1_D); \
	  $(RM) $(SHARED_PGLXLIB_NM_ALT2_D); \
	  ln $(SHARED_PGLXLIB_NM_D) $(SHARED_PGLXLIB_NM_ALT2_D); \
	  ); \
	for file in $(INSTALL_INCLUDES); do \
		$(PADS_HOME)/scripts/install-sh -c -m 444 $(PADS_HOME)/include/$$file $(INSTALLROOT)/include/; \
	done;)

clean :
	$(RM) $(STATIC_PGLXLIB_NM_O) $(SHARED_PGLXLIB_NM_O) $(STATIC_PGLXLIB_NM_D) $(SHARED_PGLXLIB_NM_D) *~ *.o ../*~

veryclean : clean
	(cd $(INSTALLROOT)/include; $(RM) $(INSTALL_INCLUDES); )

 # End of rules that are run from the arch directory
endif
