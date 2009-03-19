PaperName = ddc-jacm

all:   paper
paper: $(PaperName).ps $(PaperName).pdf

clean:	
	$(RM) *.aux *.bbl *.log *.blg $(PaperName).dvi $(PaperName).ps 
	$(RM) $(PaperName).pdf *.toc


paper.dvi:  paper.tex pads.bib related_work.tex definitions.tex encoding.tex ddc_sem.tex \
            ddc_syntax.tex intro.tex ipads.tex conclusion.tex appendix.tex \
            meta_theory.tex abstract.tex
	latex paper.tex; bibtex paper; latex paper.tex; latex paper.tex

$(PaperName).ps:  paper.dvi
	dvips paper.dvi -o paper.ps -t letter

$(PaperName).pdf : paper.dvi
	dvipdf paper.dvi $@

.PHONY: all paper clean quick $(PaperName)

