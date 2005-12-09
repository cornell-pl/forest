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

# Rule to run the pads compiler on go.p
go.c: go.p
	$(PADSC) -D__ppc__ $<

include $(PADS_HOME)/mk/rules.mk

# Rules to make go programs
go-xml: go-xml.c go.c $(INCLUDE_DEPS) $(LIB_DEPS_O)
	@$(CCExec_DYNAMIC_O)

go3-xml: go3-xml.c go3.c $(INCLUDE_DEPS) $(LIB_DEPS_O)
	@$(CCExec_DYNAMIC_O)

go3-xml-g: go3-xml.c go3.c $(INCLUDE_DEPS) $(LIB_DEPS_D)
	@$(CCExec_DYNAMIC_D)

go-query: go-query.c go.c $(INCLUDE_DEPS) $(LIB_DEPS_O)
	@$(CCExec_DYNAMIC_O)

go-accum: go-accum.c go.c $(INCLUDE_DEPS) $(LIB_DEPS_O)
	@$(CCExec_DYNAMIC_O)




clean:
	$(RM) go.[ch] go.xsd go-xml go-query
