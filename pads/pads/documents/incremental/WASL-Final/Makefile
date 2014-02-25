all: *.tex
	latex paper.tex
	latex paper.tex
	bibtex paper
	latex paper.tex
	latex paper.tex
	dvipdfm -p letter paper.dvi

clean:
	rm -f *.aux *.dvi *.bbl
semantics: semantics.tex
	latex semantics.tex
	latex semantics.tex
	dvipdf semantics
