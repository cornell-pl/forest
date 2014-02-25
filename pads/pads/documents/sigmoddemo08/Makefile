PSSRC=

#BIB=../learningpopl08/pads.bib 
BIB=pads.bib
PDFSRC=$(PSSRC:.ps=.pdf)
TEXSRC=paper.tex 
all: paper.pdf 

paper.bbl: paper.tex $(BIB)
	latex paper.tex
	bibtex paper


quick: paper.tex $(BIB)
	latex paper.tex
	dvips paper.dvi

paper.pdf: paper.bbl $(PDFSRC) $(TEXSRC) archi.eps
	latex paper.tex
	latex paper.tex
	dvips -t letter -o paper.ps paper.dvi
	ps2pdf paper.ps

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

