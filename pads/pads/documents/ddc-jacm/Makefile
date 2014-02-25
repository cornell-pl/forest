PaperName = ddc-jacm

all:   paper
paper: $(PaperName).ps $(PaperName).pdf

clean:	
	$(RM) *.aux *.bbl *.log *.blg $(PaperName).dvi $(PaperName).ps 
	$(RM) $(PaperName).pdf *.toc


SOURCES = paper.tex pads.bib related_work.tex definitions.tex encoding.tex ddc_sem.tex \
            ddc_syntax.tex intro.tex ipads.tex conclusion.tex appendix.tex \
            meta_theory.tex abstract.tex applications.tex appendix-metatheory.tex

paper.dvi:  $(SOURCES)
	latex paper.tex; bibtex paper; latex paper.tex; latex paper.tex

$(PaperName).ps:  paper.dvi
	dvips paper.dvi -o paper.ps -t letter

$(PaperName).pdf : $(SOURCES)
	pdflatex paper.tex
	bibtex paper
	pdflatex paper.tex
	pdflatex paper.tex
	mv paper.pdf $@

pdfquick: $(SOURCES)
	pdflatex paper.tex -o $@ 
	mv paper.pdf $@

.PHONY: all paper clean pdfquick $(PaperName) 

