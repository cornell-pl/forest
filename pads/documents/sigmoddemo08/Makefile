PSSRC=

PDFSRC=$(PSSRC:.ps=.pdf)
TEXSRC=paper.tex 
all: paper.pdf

paper.bbl: paper.tex ../learningpopl08/pads.bib 
	latex paper.tex
	bibtex paper

paper.pdf: paper.bbl $(PDFSRC) $(TEXSRC) archi.eps
	latex paper.tex
	latex paper.tex
	dvipdf paper.dvi

clean:
	rm -f paper.pdf
	rm -f paper.ps
	rm -f *.bbl
	rm -f *.dvi
	rm -f *.blg
	rm -f *.aux
	rm -f *.log
	rm -f *~

.SUFFIXES: .ps .pdf .jgr .eps 

.ps.pdf:
	epstopdf $<

.eps.pdf:
	ps2pdf -dEPSCrop $< 

.jgr.eps:
	jgraph $< > $@

