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
LIBS = $(LIBDIR)/libpads.a $(LIBDIR)/libast.a
INCLUDES = -I$(PADS_HOME)/padsc/include -I$(INSTALLROOT)/include/ast -I.

# Rule to run the pads compiler on node-centric.p
node-centric.c: node-centric.p
	$(PADSC) $<

data.c: data.p pget.c
	$(PADSC) $<

include $(PADS_HOME)/mk/rules.mk

# Rule to make node-centric-xml program

node-centric-xml: node-centric-xml.c node-centric.c 
	$(CC) $(INCLUDES) node-centric-xml.c node-centric.c $(LIBS) -o node-centric-xml

node-centric-fmt: node-centric-fmt.c node-centric.c 
	$(CC) $(INCLUDES) node-centric-fmt.c node-centric.c $(LIBS) -o node-centric-fmt

node-centric-sum: node-centric-sum.c node-centric.c 
	$(CC) $(INCLUDES) node-centric-sum.c node-centric.c $(LIBS) -o node-centric-sum


data-xml: data-xml.c data.c 
	$(CC) $(INCLUDES) data-xml.c data.c pget.c $(LIBS) -o data-xml

data-fmt: data-fmt.c data.c 
	$(CC) $(INCLUDES) data-fmt.c data.c pget.c $(LIBS) -o data-fmt

data-sum: data-sum.c data.c 
	$(CC) $(INCLUDES) data-sum.c data.c pget.c $(LIBS) -o data-sum



clean:
	$(RM) node-centric.[ch] node-centric.xsd node-centric-xml node-centric-accum node-centric-fmt node-centric-sum
	$(RM) data.[ch] data.xsd data-xml data-accum data-fmt data-sum
