
 # This is a GNU makefile.

ifndef PADS_HOME
  export PADS_HOME=$(shell cd ../..; pwd)
endif

ifndef AST_ARCH
  export AST_ARCH=$(shell $(PADS_HOME)/ast-ast/bin/package.cvs)
endif

CURDIR := $(shell pwd)
ifneq ($(AST_ARCH),$(notdir $(CURDIR)))
include $(PADS_HOME)/mk/redirect.mk
else

 # The following rules are run from the arch directory

SCRIPTS = $(PADS_HOME)/scripts
include $(PADS_HOME)/mk/rules.mk

VPATH = ..

INCDIR = $(PADS_HOME)/padsc/include
PCGEN_IN = $(INCDIR)/pcgen-macros.h
PCGEN = $(INCDIR)/pcgen-macros-gen.h

INSTALL_INCLUDES = io_disc.h rbuf.h pads.h pads-private.h pads-internal.h pcgen-macros-gen.h

all : $(PCGEN)

install : all
	+@(set -x ; \
	for file in $(INSTALL_INCLUDES); do \
		$(PADS_HOME)/scripts/install-sh -c -m 444 $(PADS_HOME)/padsc/include/$$file $(INSTALLROOT)/include; \
	done;)

$(PCGEN) : $(SCRIPTS)/mksrc.pl $(PCGEN_IN)
	@echo "Building pcgen-macros-gen.h"
	$(SCRIPTS)/mksrc.pl $(PADS_HOME)/padsc/include/pcgen-macros.h $(INCDIR) $(PADS_HOME)

clean :
	$(RM) *~ ../*~

veryclean :
	$(RM) *~ ../*~ $(PCGEN)
	(cd $(INSTALLROOT)/include; $(RM) $(INSTALL_INCLUDES); )

 # End of rules that are run from the arch directory
endif
