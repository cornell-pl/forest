SHELL= /bin/sh

# PADS documentation  programs
#
#
###############################################################################
# Default environment variables.
###############################################################################

## root hancock directory.

## flags to always pass to the hancock compiler
HCC_FLAGS= 

## location of padsc executable.
PADS=$(PADS_HOME)/padsc

###############################################################################
#  Documentation programs.
###############################################################################


EXAMPLE_DIR= $(PADS_HOME)/documents/manual/code
EXAMPLES= ai.p

CLEAN= 

EXTRACT= $(PADS_HOME)/documents/extract-code/extract


all:	clean code

all_check:	clean code

clean:	
	rm -rf *.tex


code:	$(EXAMPLES) Makefile 
	for i in $(EXAMPLES) ; do $(EXTRACT) $$i; done

