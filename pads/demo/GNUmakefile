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

# Rule to make wsl-xml program
wsl-xml: wsl-xml.c wsl.c 
	$(CC) $(INCLUDES) wsl-xml.c wsl.c $(LIBS) -o wsl-xml

wsl-accum: wsl-accum.c wsl.c 
	$(CC) $(INCLUDES) wsl-accum.c wsl.c $(LIBS) -o wsl-accum

wsl-fmt: wsl-fmt.c wsl.c 
	$(CC) $(INCLUDES) wsl-fmt.c wsl.c $(LIBS) -o wsl-fmt

wsl-sum: wsl-sum.c wsl.c 
	$(CC) $(INCLUDES) wsl-sum.c wsl.c $(LIBS) -o wsl-sum

clean:
	$(RM) wsl.[ch] wsl-xml wsl-accum wsl-fmt wsl-sum
