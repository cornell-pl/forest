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
PEXAMPLES= ai.p pstruct.p punion.p parray.p opt.p ptypedef.p enum2.p wsl.p sirius.p
CEXAMPLES= ai-annotated.h punion-annotated.h opt-annotated.h typedef-annotated.h enum2-annotated.h sirius-filter.c wsl-accum.c wsl-fmt.c sirius-xml.c dibbler_new_annotated.h parse-state.h library.h

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
	for i in $(EXAMPLES) ; do $(EXTRACT) -hevea $$i; done
