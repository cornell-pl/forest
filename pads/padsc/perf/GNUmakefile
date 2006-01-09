
 # This is a GNU makefile.

ifndef PADS_HOME
  export PADS_HOME=$(shell cd ../..; pwd)
endif

SUBDIRS = ai_big

all:
	+@(for subdir in $(SUBDIRS); do \
		(cd $$subdir; $(MAKE)); \
	done)

regress:
	+@(for subdir in $(SUBDIRS); do \
		(cd $$subdir; $(MAKE) regress); \
	done)

clean:
	+@(for subdir in $(SUBDIRS); do \
		(cd $$subdir; $(MAKE) clean); \
	done)

veryclean:
	+@(for subdir in $(SUBDIRS); do \
		(cd $$subdir; $(MAKE) veryclean); \
	done)
