
 # This is a GNU makefile.

PADSC=$(PADS_HOME)/scripts/padsc
AST_ARCH := $(shell $(PADS_HOME)/ast-ast/bin/package.cvs)
INSTALLROOT=$(PADS_HOME)/ast-ast/arch/$(AST_ARCH)
LIBDIR=$(INSTALLROOT)/lib
LIBS = $(LIBDIR)/libpads.a $(LIBDIR)/libast.a
INCLUDES = -I$(PADS_HOME)/padsc/include -I$(INSTALLROOT)/include/ast -I.
SOURCES = paper.tex pads.bib definitions.tex abstract.tex code.sty conclusion.tex vcodex.tex \
	experiments.tex generated-compression.tex intro.tex pads.tex related.tex

all:	paper

%.c: %.p
	$(PADSC) $<

clean:	
	rm -rf *.aux *.bbl *.log *.blg *.dvi *.ps *.pdf *.toc _region_.tex

quick:  $(SOURCES)
	latex paper.tex; dvips paper.dvi -t letter -o paper.ps

pdfquick:  $(SOURCES)
	pdflatex paper.tex

paper:  $(SOURCES)
	latex paper.tex; bibtex paper; latex paper.tex; latex paper.tex; dvips paper.dvi -o paper.ps -t letter
	pdflatex paper.tex


