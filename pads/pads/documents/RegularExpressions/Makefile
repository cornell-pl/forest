all: RE.pdf

.PRECIOUS: ProtectionProfile.pdf

di:
	rm -f Diag.pdf
	rm -f Diag.aux
	rm -f Diag.out
	rm -f Diag.dvi
	rm -f Diag.log
	make Diag.pdf

try:    Try.tex
	rm -f Try.pdf
	rm -f Try.aux
	rm -f Try.out
	rm -f Try.dvi
	rm -f Try.log
	make Try.pdf

# Run twice to resolve references, but does
# not do anything with the bibliography
# *** Running twice is currently commented out
%.pdf: %.tex
	pdflatex $<
#	rm -f $@
#	pdflatex $<

# Generate the bibliography
%.bbl: %.pdf
	bibtex $*
	pdflatex $*
	pdflatex $*

RE.pdf: CalculatingEntropy.tex MetricSpaces.tex Graphs.tex Shifts.tex

clean:
	rm -f RE.pdf Diag.pdf
	rm -f *.log *.aux *.out *.dvi *.toc *.bbl *.blg *.lof *.lot *~
	rm -f Try111.tex Try*.pdf
