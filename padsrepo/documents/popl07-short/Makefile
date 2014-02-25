
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

# include $(PADS_HOME)/mk/rules.mk

clean:	
	rm -rf *.aux *.bbl *.log *.blg *.dvi *.ps *.pdf *.toc _region_.tex

quick:  paper.tex
	latex paper.tex; dvips paper.dvi -t letter -o paper.ps

pdfquick:  paper.tex
	pdflatex paper.tex

pdfpaper: paper.tex intro.tex pads.bib pml_overview.tex impl.tex conc.tex \
	darkstar_pml.tex newick_pml.tex sirius_pml.tex gentool.tex \
	related_work.tex definitions.tex
	pdflatex paper.tex; bibtex paper; pdflatex paper.tex; pdflatex paper.tex

paper:  paper.tex pads.bib related_work.tex definitions.tex
	latex paper.tex; bibtex paper; latex paper.tex; latex paper.tex; dvips paper.dvi -o paper.ps -t letter
	pdflatex paper.tex


pp.pdf: pp.tex ddc_pp.tex appendix_pp.tex
	pdflatex pp.tex
