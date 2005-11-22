PSSRC=

PDFSRC=$(PSSRC:.ps=.pdf)
TEXSRC=paper.tex 
all: paper.pdf

paper.bbl: intro.tex pads.tex discussion.tex paper.tex ../pads.bib ../galax.bib
	pdflatex paper.tex
	bibtex paper

#paper.pdf: paper.ps 
#	ps2pdf paper.ps

paper.pdf: paper.bbl $(PDFSRC) $(TEXSRC) padx-arch.ps padx-arch1.ps
	pdflatex paper.tex
	pdflatex paper.tex
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

.SUFFIXES: .ps .pdf .jgr .eps 

.ps.pdf:
	epstopdf $<

.eps.pdf:
	ps2pdf -dEPSCrop $< 

.jgr.eps:
	jgraph $< > $@

