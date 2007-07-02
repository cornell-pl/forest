
 # This is a GNU makefile.

all:	paper


clean:	
	rm -rf *.aux *.log *.blg *.dvi *.ps *.pdf *.toc

quick:  paper.tex
	latex paper.tex; dvips paper.dvi -o paper.ps

quick2:  paper.tex
	latex paper.tex; dvipdf paper.dvi

paper:  paper.tex pads.bib discussion.tex definitions.tex intro.tex review.tex algorithm.tex conclusion.tex appendix.tex abstract.tex
	latex paper.tex; 
	#bibtex paper.text;
	latex paper.tex; latex paper.tex; dvips paper.dvi -o paper.ps -t letter
	dvipdf paper.dvi
