SHELL= /bin/sh

# PADS documentation  programs
#
#
###############################################################################
# Default environment variables.
###############################################################################

PADSC= $(PADS_HOME)/scripts/padsc

###############################################################################
#  Documentation programs.
###############################################################################


EXAMPLE_DIR= $(PADS_HOME)/documents/manual/code
PEXAMPLES= ai.p pstruct.p punion.p
CEXAMPLES= ai-annotated.h
EXAMPLES = $(PEXAMPLES) $(CEXAMPLES)

CLEAN= 

EXTRACT= $(PADS_HOME)/documents/extract-code/extract

OUTPUT=gen/

all:	clean code

check:	$(PEXAMPLES) Makefile 
	for i in $(PEXAMPLES) ; do $(PADSC) -r $(OUTPUT) $$i; done

clean:	
	rm -rf *.tex
	rm -rf gen/*


code:	$(EXAMPLES) Makefile 
	for i in $(EXAMPLES) ; do $(EXTRACT) $$i; done

