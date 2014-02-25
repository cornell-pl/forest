
 # This is a GNU makefile.

all:	paper


clean:	
	rm -rf *.aux *.bbl *.log *.blg *.dvi *.ps *.pdf *.toc

quick:  paper.tex
	latex semantics.tex; dvips semantics.dvi -o semantics.ps

paper:  paper.tex pads.bib related_work.tex definitions.tex encoding.tex ddc_sem.tex ddc_syntax.tex intro.tex ipads.tex conclusion.tex appendix.tex abstract.tex
	latex paper.tex; bibtex paper; latex paper.tex; latex paper.tex; dvips paper.dvi -o paper.ps -t letter
	pdflatex paper.tex
