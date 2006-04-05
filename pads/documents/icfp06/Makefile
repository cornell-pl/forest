
 # This is a GNU makefile.

PADSC=$(PADS_HOME)/scripts/padsc
AST_ARCH := $(shell $(PADS_HOME)/ast-ast/bin/package.cvs)
INSTALLROOT=$(PADS_HOME)/ast-ast/arch/$(AST_ARCH)
LIBDIR=$(INSTALLROOT)/lib
LIBS = $(LIBDIR)/libpads.a $(LIBDIR)/libast.a
INCLUDES = -I$(PADS_HOME)/padsc/include -I$(INSTALLROOT)/include/ast -I.

all:	paper

%.c: %.p
	$(PADSC) $<

include $(PADS_HOME)/mk/rules.mk

rwxml_regulus: rwxml_regulus.c regulus.c $(INCLUDE_DEPS) $(LIB_DEPS_O)
	@$(CCExec_DYNAMIC_O)

rwxml_regulus_d: rwxml_regulus.c regulus.c $(INCLUDE_DEPS) $(LIB_DEPS_D)
	@$(CCExec_DYNAMIC_D)

clean:	
	rm -rf *.aux *.bbl *.log *.blg *.dvi *.ps *.pdf *.toc

quick:  paper.tex
	latex paper.tex; dvips paper.dvi -t letter -o paper.ps

paper:  paper.tex pads.bib related_work.tex definitions.tex ddcpoly.tex appendix.tex 
	latex paper.tex; bibtex paper; latex paper.tex; latex paper.tex; dvips paper.dvi -o paper.ps -t letter
	pdflatex paper.tex
