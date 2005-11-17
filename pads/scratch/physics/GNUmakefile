 # This makefile uses gmake extensions
 # such as $(shell ...) to execute a command

.PRECIOUS: %.c

# These variables need to be set
ifndef PADS_HOME
  PADS_HOME=/home/kfisher/pads
endif

# These variables should not need to change
PADSC=$(PADS_HOME)/scripts/padsc
AST_ARCH := $(shell $(PADS_HOME)/ast-ast/bin/package.cvs)
INSTALLROOT=$(PADS_HOME)/ast-ast/arch/$(AST_ARCH)
LIBDIR=$(INSTALLROOT)/lib
LIBS = $(LIBDIR)/libpads.a $(LIBDIR)/libast.a
INCLUDES = -I$(PADS_HOME)/padsc/include -I$(INSTALLROOT)/include/ast -I.

PADSOPTIONS= -D__ppc__ 
ifdef USE_GALAX
PADSOPTIONS+=-x
endif

# Rule to run the pads compiler on go.p
catalog.c: catalog.p
	$(PADSC) $(PADSOPTIONS) $<

catalog2.c: catalog2.p
	$(PADSC) $(PADSOPTIONS) $<

include $(PADS_HOME)/mk/rules.mk

# Rules to make catalog programs
catalog-xml: catalog-xml.c catalog.c $(INCLUDE_DEPS) $(LIB_DEPS_O)
	@$(CCExec_DYNAMIC_O)

catalog-query: catalog-query.c catalog.c $(INCLUDE_DEPS) $(LIB_DEPS_O)
	@$(CCExec_DYNAMIC_O)

catalog-accum: catalog-accum.c catalog.c $(INCLUDE_DEPS) $(LIB_DEPS_O)
	@$(CCExec_DYNAMIC_O)

catalog2-xml: catalog2-xml.c catalog2.c $(INCLUDE_DEPS) $(LIB_DEPS_O)
	@$(CCExec_DYNAMIC_O)

catalog2-xml-g: catalog2-xml.c catalog2.c $(INCLUDE_DEPS) $(LIB_DEPS_D)
	@$(CCExec_DYNAMIC_D)




clean:
	$(RM) catalog.[ch] catalog.xsd catalog-xml catalog-query
