
 # This is a GNU makefile.

all:	paper


clean:	
	rm -rf *.aux *.log *.blg *.dvi *.ps *.toc

quick: 	paper.tex
	latex paper.tex; dvips paper.dvi -o paper.ps

quick2:	paper.tex
	pdflatex paper.tex

paper:  paper.tex pads.bib abstract.tex
	pdflatex paper.tex; 
	bibtex paper;
	pdflatex paper.tex; 
	pdflatex paper.tex

nate:  paper.tex pads.bib abstract.tex
	xelatex paper.tex; 
	bibtex paper;
	xelatex paper.tex; 
	xelatex paper.tex
