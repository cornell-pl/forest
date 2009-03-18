PaperName = ddc-jacm

all:   paper
paper: $(PaperName).ps $(PaperName).pdf

clean:	
	$(RM) *.aux *.bbl *.log *.blg $(PaperName).dvi $(PaperName).ps 
	$(RM) $(PaperName).pdf *.toc

quick:  paper.tex
	latex semantics.tex; dvips semantics.dvi -o semantics.ps

$(PaperName).ps:  paper.tex pads.bib related_work.tex definitions.tex encoding.tex ddc_sem.tex \
                 ddc_syntax.tex intro.tex ipads.tex conclusion.tex appendix.tex abstract.tex
	latex paper.tex; bibtex paper; latex paper.tex; latex paper.tex; dvips paper.dvi -o paper.ps -t letter

$(PaperName).pdf : paper.dvi
	dvipdf paper.dvi $@

.PHONY: all paper clean quick $(PaperName)

