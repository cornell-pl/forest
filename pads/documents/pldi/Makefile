
 # This is a GNU makefile.

SHELL= /bin/sh

ifndef PADS_HOME
  export PADS_HOME=$(shell cd ../..; pwd)
endif

# PADS documentation  programs
#
#
###############################################################################
# Default environment variables.
###############################################################################

## root pads directory.

## flags to always pass to the pads compiler
PADS_FLAGS= 

## location of padsc executable.
PADS=$(PADS_HOME)/scripts/padsc

###############################################################################
#  Documentation programs.
###############################################################################


EXAMPLE_DIR= $(PADS_HOME)/padsc/examples/p
PEXAMPLES= $(EXAMPLE_DIR)/wsl.p $(EXAMPLE_DIR)/dibbler_new.p
CEXAMPLES= 
EXAMPLES= $(PEXAMPLES) $(CEXAMPLES)

CLEAN= 

EXTRACT= $(PADS_HOME)/documents/extract-code/extract

OUTPUT=gen/

all:	docode paper

all_check:	clean code

clean:	
	rm -rf *.aux *.bbl *.log *.blg *.dvi *.ps *.pdf *.toc

check:	$(PEXAMPLES) Makefile
	echo Checking pads source code for paper
	for i in $(PEXAMPLES) ; do $(PADS) -r $(OUTPUT) $$i; done

code:	$(EXAMPLES) Makefile
	echo Generating code for paper
	for i in $(EXAMPLES) ; do $(EXTRACT) $$i; done

docode:	check code


extract: ../extract-code/pads-keywords.sml
	(cd ../extract-code; $(MAKE))

paper:
	latex paper.tex; latex paper.tex; bibtex paper; latex paper.tex; dvips paper.dvi -o paper.ps -t letter
	pdflatex paper.tex
