
 # This is a GNU makefile.

all:	paper


clean:	
	rm -rf *.aux *.bbl *.log *.blg *.dvi *.ps *.pdf *.toc

quick:  proposal.tex
	latex proposal.tex; dvips proposal.dvi
paper:  proposal.tex pads.bib
	latex proposal.tex; latex proposal.tex; bibtex proposal; latex proposal.tex; dvips proposal.dvi -o proposal.ps -t letter
	dvipdf proposal
bio:  dpw-bio.tex
	latex dpw-bio.tex; dvips dpw-bio.dvi; pdflatex dpw-bio.tex
