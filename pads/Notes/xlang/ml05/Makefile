
 # This is a GNU makefile.

all:	paper


clean:	
	rm -rf *.aux *.bbl *.log *.blg *.dvi *.ps *.pdf *.toc

quick:  proposal.tex
	latex paper.tex; dvips paper.dvi
paper:  paper.tex pads.bib
	latex paper.tex; bibtex paper; latex paper.tex; latex paper.tex; dvips proposal.dvi -o proposal.ps -t letter
	pdflatex proposal.tex
