
.SUFFIXES: .dvi .ps

PAPER = paper
LATEX = latex
TEXFILES = *.tex
BIBFILE = pads.bib

default: $(PAPER).ps $(BIBFILE)

eps:	$(EPSFILES)

$(PAPER).dvi: $(TEXFILES) $(EPSFILES) $(BIBFILE)
	$(LATEX) $(PAPER)
	bibtex $(PAPER)
	$(LATEX) $(PAPER)
	$(LATEX) $(PAPER)

$(PAPER).pdf: $(PAPER).dvi
	dvipdf $(PAPER).dvi $(PAPER).pdf

$(PAPER).ps: $(PAPER).dvi
	dvips -o $(PAPER).ps $(PAPER).dvi

dvi: $(PAPER).dvi

ps: $(PAPER).ps

pdf: $(PAPER).pdf

print: $(PAPER).ps
	lpr -Pxrx_432/duplex $(PAPER).ps

clean:
	rm -f *.ps *.dvi *.aux *.bbl *.blg *.log *.pdf *~
    
wc:
	wc -w $(TEXFILES)

ispell:
	ispell -b -t $(TEXFILES)
