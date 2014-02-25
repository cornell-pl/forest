
 # This is a GNU makefile.

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

paper:  paper.tex pads.bib related_work.tex definitions.tex ddcpoly.tex appendix.tex 
	latex paper.tex; bibtex paper; latex paper.tex; latex paper.tex; dvips paper.dvi -o paper.ps -t letter
	pdflatex paper.tex


pp.pdf: pp.tex ddc_pp.tex appendix_pp.tex
	pdflatex pp.tex
