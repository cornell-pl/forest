
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


EXAMPLE_DIR= $(PADS_HOME)/documents/manual/code
EXAMPLES= ai.p

CLEAN= 

EXTRACT= $(PADS_HOME)/documents/extract-code/extract
HEVEA_PATH =	/usr/local/bin
HEVEA = 	$(HEVEA_PATH)/hevea
HEVEA_FILES = code.hva manual.hva 
HEVEA_FLAGS = 	-noiso -s -exec xxdate.exe  
HACHA = 	$(HEVEA_PATH)/hacha
HACHA_FLAGS =	


all:	docode manual

all_check:	clean code

clean:	
	rm -rf *.aux *.bbl *.log *.blg *.dvi *.ps *.pdf *.toc

docode:	extract
	echo Generating code for manual
	(cd code; $(MAKE))

extract: ../extract-code/pads-keywords.sml
	(cd ../extract-code; $(MAKE))

manual:
	pdflatex manual.tex

manual-all: docode
	pdflatex manual.tex; bibtex manual
	pdflatex manual.tex; pdflatex manual.tex

manual_html:
	(cd code; $(MAKE))
	$(HEVEA) $(HEVEA_FLAGS) $(HEVEA_FILES) manual.tex
	$(HACHA) manual.html