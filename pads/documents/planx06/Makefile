PSSRC=

PDFSRC=$(PSSRC:.ps=.pdf)
TEXSRC=paper.tex defs.tex 

all: paper.pdf

paper.bbl: paper.tex ../pldi/pads.bib
	latex paper.tex
	bibtex paper

paper.pdf: paper.ps
	ps2pdf paper.ps

paper.ps: paper.bbl $(PDFSRC) $(TEXSRC)
	latex paper.tex
	latex paper.tex
	dvips -t letter paper.dvi -o

clean:
	rm -f paper.pdf
	rm -f paper.ps
	rm -f *.bbl
	rm -f *.dvi
	rm -f *.blg
	rm -f *.aux
	rm -f *.log
	rm -f *~

.SUFFIXES: .ps .pdf

.ps.pdf:
	epstopdf $<

