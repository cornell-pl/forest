
 # This is a GNU makefile.

all:	paper


clean:	
	rm -rf *.aux *.bbl *.log *.blg *.dvi *.ps *.pdf *.toc

quick:  proposal.tex
	latex paper.tex; dvips paper.dvi
paper:  proposal.tex pads.bib
	latex proposal.tex; latex proposal.tex; bibtex proposal; latex proposal.tex; dvips proposal.dvi -o proposal.ps -t letter
	pdflatex proposal.tex
