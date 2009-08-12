all: *.tex pads.bib
	pdflatex paper.tex

paper: *.tex
	latex paper.tex
	latex paper.tex
	bibtex paper
	latex paper.tex
	latex paper.tex
	dvipdf paper.dvi


clean:
	rm -f *.aux *.dvi *.bbl
