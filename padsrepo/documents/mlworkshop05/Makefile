
 # This is a GNU makefile.

all:	paper


clean:	
	rm -rf *.aux *.bbl *.log *.blg *.dvi *.ps *.pdf *.toc

quick:  paper.tex
	latex paper.tex; dvips paper.dvi -o paper.ps

paper:  paper.tex pads.bib
	latex paper.tex; bibtex paper; latex paper.tex; latex paper.tex; dvips paper.dvi -o paper.ps -t letter
	pdflatex paper.tex
