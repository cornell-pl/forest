PSSRC=

PDFSRC=$(PSSRC:.ps=.pdf)
TEXSRC=paper.tex 
all: paper.pdf

paper.bbl: intro.tex pads.tex discussion.tex paper.tex ../pads.bib ../galax.bib 
	pdflatex paper.tex
	bibtex paper

paper.pdf: paper.bbl $(PDFSRC) $(TEXSRC) launch.pdf arch2.pdf
	pdflatex paper.tex
	pdflatex paper.tex

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

