.phony: all xelatex clean

TARGET = main.pdf

SOURCES = main.tex \
	  main.bib \
	  background.tex \
          posix.tex
	  introduction.tex \
          txforest.tex \
	  main.bbl

all : $(TARGET)

%.pdf : $(SOURCES)
	pdflatex $(basename $<)
	-bibtex $(basename $<)
	pdflatex $(basename $<)
	pdflatex $(basename $<)

xelatex: $(SOURCES)
	xelatex $(basename $<)
	-bibtex $(basename $<)
	xelatex $(basename $<)
	xelatex $(basename $<)

clean:
	rm -fr main.pdf main.log main.aux 
