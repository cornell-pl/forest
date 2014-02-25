all:
	latex paper.tex
	latex paper.tex
	bibtex paper
	latex paper.tex
	latex paper.tex
	dvipdfm -p letter paper.dvi

ps:
	latex paper.tex
	bibtex paper
	latex paper.tex
	dvips paper.dvi

clean:
	rm -f *.aux *.dvi *.bbl *.blg

