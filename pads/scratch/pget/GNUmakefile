 # This makefile uses gmake extensions
 # such as $(shell ...) to execute a command

.PRECIOUS: %.c

# These variables need to be set
ifndef PADS_HOME
  PADS_HOME=~frances/research/pads
endif

# These variables should not need to change
PADSC=$(PADS_HOME)/scripts/padsc
AST_ARCH := $(shell $(PADS_HOME)/ast-ast/bin/package.cvs)
INSTALLROOT=$(PADS_HOME)/ast-ast/arch/$(AST_ARCH)
LIBDIR=$(INSTALLROOT)/lib
LIBS = $(LIBDIR)/libpads.a $(LIBDIR)/libast.a $(OS_SPEC_XTRA_LIBS)
INCLUDES = -I$(PADS_HOME)/padsc/include -I$(INSTALLROOT)/include/ast -I.

# Rule to run the pads compiler on data-acc.p
data.c: data.p
	$(PADSC) $<

include $(PADS_HOME)/mk/rules.mk

# Rule to make data-xml program

data-xml: data-xml.c data.c 
	$(CC) $(INCLUDES) data-xml.c data.c pget.c $(LIBS) -o data-xml

data-fmt: data-fmt.c data.c 
	$(CC) $(INCLUDES) data-fmt.c data.c pget.c $(LIBS) -o data-fmt

data-acc: data-acc.c data.c 
	$(CC) $(INCLUDES) data-acc.c data.c pget.c $(LIBS) -o data-acc




clean:
	$(RM) data.[ch] data.xsd data-xml data-accum data-fmt data-acc
