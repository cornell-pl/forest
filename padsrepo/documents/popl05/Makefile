
 # This is a GNU makefile.

all:	semantics


clean:	
	rm -rf *.aux *.bbl *.log *.blg *.dvi *.ps *.pdf *.toc

quick:  semantics.tex
	latex semantics.tex; dvips semantics.dvi -o semantics.ps

semantics:  semantics.tex pads.bib related_work.tex definitions.tex encoding.tex ddc_sem.tex ddc_syntax.tex intro.tex ipads.tex conclusion.tex appendix.tex abstract.tex
	latex semantics.tex; bibtex semantics; latex semantics.tex; latex semantics.tex; dvips semantics.dvi -o semantics.ps -t letter
	pdflatex semantics.tex
