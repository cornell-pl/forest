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

# Rule to run the pads compiler on wsl.p
wsl.c: wsl.p
	$(PADSC) $<

sirius.c: sirius.p
	$(PADSC) $<

go.c: go.p
	$(PADSC) -x -D__ppc__ $<

include $(PADS_HOME)/mk/rules.mk

# Rule to make wsl-xml program
wsl-xml: wsl-xml.c wsl.c 
	$(CC) $(INCLUDES) wsl-xml.c wsl.c $(LIBS) -o wsl-xml

wsl-accum: wsl-accum.c wsl.c 
	$(CC) $(INCLUDES) wsl-accum.c wsl.c $(LIBS) -o wsl-accum

wsl-fmt: wsl-fmt.c wsl.c 
	$(CC) $(INCLUDES) wsl-fmt.c wsl.c $(LIBS) -o wsl-fmt

wsl-sum: wsl-sum.c wsl.c 
	$(CC) $(INCLUDES) wsl-sum.c wsl.c $(LIBS) -o wsl-sum


# Rule to make sirius-xml program
sirius-xml: sirius-xml.c sirius.c 
	$(CC) $(INCLUDES) sirius-xml.c sirius.c $(LIBS) -o sirius-xml

sirius-accum: sirius-accum.c sirius.c 
	$(CC) $(INCLUDES) sirius-accum.c sirius.c $(LIBS) -o sirius-accum

sirius-fmt: sirius-fmt.c sirius.c 
	$(CC) $(INCLUDES) sirius-fmt.c sirius.c $(LIBS) -o sirius-fmt

sirius-filter: sirius-filter.c sirius.c 
	$(CC) $(INCLUDES) sirius-filter.c sirius.c $(LIBS) -o sirius-filter

# Rules to make go programs
go-xml: go-xml.c go.c $(INCLUDE_DEPS) $(LIB_DEPS_O)
	@$(CCExec_DYNAMIC_O)

go-query: go-query.c go.c $(INCLUDE_DEPS) $(LIB_DEPS_O)
	@$(CCExec_DYNAMIC_O)

go-accum: go-accum.c go.c $(INCLUDE_DEPS) $(LIB_DEPS_O)
	@$(CCExec_DYNAMIC_O)




clean:
	$(RM) wsl.[ch] wsl.xsd wsl-xml wsl-accum wsl-fmt wsl-sum
	$(RM) sirius.[ch] sirius.xsd sirius-xml sirius-accum sirius-fmt sirius-filter sirius_clean sirius_errors
	$(RM) go.[ch] go.xsd go-xml go-query
