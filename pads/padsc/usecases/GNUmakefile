
 # This is a GNU makefile.

ifndef PADS_HOME
  export PADS_HOME=$(shell cd ../..; pwd)
endif

SUBDIRS = simple wsl sirius syslog simple2

all:
	+@(for subdir in $(SUBDIRS); do \
		(cd $$subdir; $(MAKE) -f GNUMakefile ); \
	done)

regress:
	+@(for subdir in $(SUBDIRS); do \
		(cd $$subdir; $(MAKE) -f GNUMakefile regress); \
	done)

clean:
	+@(for subdir in $(SUBDIRS); do \
		(cd $$subdir; $(MAKE) -f GNUMakefile clean); \
	done)

veryclean:
	+@(for subdir in $(SUBDIRS); do \
		(cd $$subdir; $(MAKE) -f GNUMakefile veryclean); \
	done)
