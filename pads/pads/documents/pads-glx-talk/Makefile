PSSRC=padx-overview.ps galax_overview.ps

PDFSRC=$(PSSRC:.ps=.pdf)
TEXSRC=talk.tex

all: talk.pdf

tools2005.pdf : tools2005.tex $(PDFSRC) $(TEXSRC)
	pdflatex tools2005.tex

talk.pdf: talk.tex $(PDFSRC) $(TEXSRC)
	pdflatex talk.tex

clean:
	rm -f *.pdf
	rm -f *.bbl
	rm -f *.blg
	rm -f *.aux
	rm -f *.log

.SUFFIXES: .ps .pdf

.ps.pdf:
	epstopdf $<

