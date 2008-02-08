ifndef LEARN_HOME
%:	forceabort
	@echo "ERROR: environment variable LEARN_HOME must contain path to root of PADS inference system."
	@exit 1
forceabort: ;
endif

all:
	@(echo "Making the learning tool.")
	@(cd src; $(MAKE) -f GNUmakefile all)

clean:
	@(echo "Cleaning")
	cd src; $(MAKE) -f GNUmakefile clean
	cd examples; $(MAKE) -f GNUmakefile clean
